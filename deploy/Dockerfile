# Statement Studio, one container per analyst.
#
# WHY THIS EXISTS. `shiny::runApp()` and open-source Shiny Server both run ONE R
# process for the whole app, so every analyst shares one memory space, one
# `tempdir()` and one set of globals. The app is written correctly against that --
# all per-user state lives inside `server()` -- but correctness there is a property
# of the code, re-established on every commit, not a boundary the operating system
# enforces. Every uploaded statement is also owned by the same service account, so
# NTFS cannot tell one investigator from another: neither of them is the process
# identity.
#
# ShinyProxy starts a container per session. Globals cannot leak because there is
# no shared process, and each container mounts only its own volume, so the file
# system does the enforcing and a bug in the app's own checks stops being a
# cross-case evidence leak. See docs/operational/one-container-per-analyst.md.
#
# NOT BUILT OR RUN IN THE SESSION THAT WROTE IT -- no container runtime was
# available. The PACKAGE LIST IS MEASURED, not guessed: it is `loadedNamespaces()`
# after a real CSV conversion and a real PDF conversion, minus the base packages.
# Verify with the one command at the end of this file before trusting it.

# r-base pinned to the version the suite is green on. NOT `latest`: an R minor
# version arriving silently is exactly the kind of change that moves a figure, and
# a forensic tool has to be able to say which R produced a number.
FROM r-base:4.3.3

# ---- system libraries ------------------------------------------------------
#
# poppler-utils is FOUR binaries this app needs, not one:
#   pdftoppm   -- rasterise a page for the OCR path and the redaction scan
#   pdftocairo -- read SIGNS DRAWN AS INK. Without it the reader fails QUIET: a
#                 minus drawn as a line, or printed in the background colour, is
#                 invisible and the statement is read with the signs INVERTED,
#                 with nothing to catch it on a statement that prints no running
#                 balance. scripts/health-check.R asks for it by name.
#   pdftotext  -- the text layer
#   pdfinfo    -- page geometry and rotation
# tesseract + the English data read a scanned statement.
# libpoppler-cpp-dev is what the `pdftools` R package links against.
RUN apt-get update && apt-get install -y --no-install-recommends \
      poppler-utils \
      tesseract-ocr tesseract-ocr-eng \
      libpoppler-cpp-dev \
      libmagick++-dev \
      libssl-dev libxml2-dev libcurl4-openssl-dev \
      libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
      libjpeg-dev libpng-dev libtiff5-dev \
      zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

# ---- R packages ------------------------------------------------------------
#
# THE DIRECT ELEVEN, measured rather than grepped: these are what our own code
# names. Their dependencies come with them (R6 Rcpp askpass cli ellipsis fastmap
# htmlwidgets httpuv later lifecycle magrittr mime promises qpdf rlang stringi
# xtable), so they are not listed twice -- a hand-maintained transitive list goes
# stale and then lies.
#
# install2.r fails the BUILD on a package that does not install. Without -e a
# missing package is a warning, the image builds, and the failure arrives later as
# a conversion that errors on a server nobody is watching.
RUN install2.r --error --skipinstalled \
      shiny DT yaml jsonlite digest openssl openxlsx pdftools magick zip htmltools \
 && rm -rf /tmp/downloaded_packages

# readxl is only needed to read an .xlsx STATEMENT (R/read_input.R). Kept separate
# because a site that never receives one can drop this line and the image shrinks.
RUN install2.r --error --skipinstalled readxl && rm -rf /tmp/downloaded_packages

# ---- the app ---------------------------------------------------------------
WORKDIR /srv/statement-studio
COPY R/            R/
COPY templates/    templates/
COPY dictionaries/ dictionaries/
COPY config/       config/
COPY www/          www/
COPY scripts/      scripts/
COPY app.R ui_labels.R ui_content.R run.R VERSION ./

# WRITABLE STATE IS MOUNTED, NEVER BAKED IN. uploads/ holds a copy of someone's
# bank statement and logs/ holds the audit trail; both must outlive the container
# and must be the mount ShinyProxy gives THIS user, not a layer shared by every
# image. Created empty so the app does not have to be the thing that discovers it
# cannot write.
RUN mkdir -p uploads logs feed requests templates/statements_user \
 && chmod 0770 uploads logs feed requests templates/statements_user

# ---- run as nobody in particular ------------------------------------------
#
# A container that runs as root gives a bug in the app root on the mounted volume,
# which is the one place real evidence lives. 10001 rather than a name so the uid
# is stable across rebuilds and the host's volume permissions keep matching it.
RUN useradd -u 10001 -m -s /usr/sbin/nologin studio \
 && chown -R studio:studio /srv/statement-studio
USER studio

# ---- how it listens -------------------------------------------------------
#
# 0.0.0.0 INSIDE THE CONTAINER IS CORRECT, and is the opposite of the rule on a
# bare Windows host. The container's network namespace has exactly one thing in it
# that may connect: ShinyProxy. There is no bypass to close, which is the whole
# point of this deployment -- on the Windows host the same setting is what let an
# identity header be forged (docs/operational/who-is-using-it.md).
#
# ONE thread: the cap in R/jobs.R exists because tesseract is OpenMP-parallel and
# takes a thread per core by default, so three concurrent scans on four cores had
# not finished a single PAGE after ten minutes. Here the container is the unit of
# concurrency, so the per-container limit is one and ShinyProxy's own limits decide
# how many analysts run at once.
ENV OMP_THREAD_LIMIT=1 \
    OMP_NUM_THREADS=1 \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8
EXPOSE 3838

# A container that is up but cannot convert is worse than one that is down, because
# nothing restarts it. This asks the app's own health check, which covers the
# settings file, the admin password, the templates, the folders, the scan tools and
# -- the one that matters most here -- whether the sign-reading binaries are
# present.
HEALTHCHECK --interval=60s --timeout=30s --start-period=45s --retries=3 \
  CMD Rscript scripts/health-check.R > /dev/null || exit 1

CMD ["R", "-e", "shiny::runApp('/srv/statement-studio', host='0.0.0.0', port=3838, launch.browser=FALSE)"]

# ---------------------------------------------------------------------------
# BUILD AND PROVE IT, on a machine with a container runtime:
#
#   docker build -f deploy/Dockerfile -t statement-studio:1.9.0 .
#
#   # 1. the health check passes INSIDE the image -- settings, templates, folders,
#   #    scan tools, and the sign-reading binaries
#   docker run --rm statement-studio:1.9.0 Rscript scripts/health-check.R
#
#   # 2. the SUITE passes inside the image. This is the one that matters: it proves
#   #    the image reads a statement the same way the machine it was built on does.
#   docker run --rm -e BSO_ALLOW_SKIPS=1 statement-studio:1.9.0 \
#       Rscript tests/run_tests.R          # needs COPY tests/ added for this run
#
#   # 3. it serves
#   docker run --rm -p 3838:3838 statement-studio:1.9.0
#   curl -sS -o /dev/null -w '%{http_code}\n' http://localhost:3838/
#
# FOR THE AIR-GAPPED SERVER, build on a machine with internet and carry the image:
#   docker save statement-studio:1.9.0 | gzip > statement-studio-1.9.0.tar.gz
#   # ...copy, then on the offline host:
#   gunzip -c statement-studio-1.9.0.tar.gz | docker load
# ---------------------------------------------------------------------------
