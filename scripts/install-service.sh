#!/usr/bin/env bash
# install-service.sh -- make the Bank Statement OCR app start AUTOMATICALLY on
# every server boot (Linux / systemd). Run ONCE, as root:
#
#   sudo bash scripts/install-service.sh            # web app on :8100, auto-start
#   sudo bash scripts/install-service.sh --inbox    # ALSO run the folder poller
#
# After this the app comes online by itself whenever the machine powers on, and
# restarts itself if it ever crashes. Nothing else to schedule.
#
#   Check it:    systemctl status bankstatements
#   Watch logs:  journalctl -u bankstatements -f
#   Stop it:     sudo systemctl stop bankstatements
#   Update:      replace the folder, then  sudo systemctl restart bankstatements
#   Remove it:   sudo systemctl disable --now bankstatements \
#                  && sudo rm /etc/systemd/system/bankstatements*.service \
#                  && sudo systemctl daemon-reload
#
# Env overrides: BSO_PORT (default 8100), BSO_USER (default the sudo caller).
set -euo pipefail

if [[ "${EUID}" -ne 0 ]]; then
  echo "Please run with sudo:  sudo bash scripts/install-service.sh" >&2
  exit 1
fi

APP_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PORT="${BSO_PORT:-8100}"
SVC_USER="${BSO_USER:-${SUDO_USER:-root}}"
RSCRIPT="$(command -v Rscript || true)"
if [[ -z "${RSCRIPT}" ]]; then
  echo "Rscript not found on PATH. Run scripts/setup.sh first." >&2
  exit 1
fi

echo "==> App folder : ${APP_DIR}"
echo "==> Runs as    : ${SVC_USER}"
echo "==> Web port   : ${PORT}"

# Make sure the folders the service writes to exist and are owned by SVC_USER.
mkdir -p "${APP_DIR}/logs" "${APP_DIR}/out" \
         "${APP_DIR}/inbox" "${APP_DIR}/outbox" "${APP_DIR}/processed" "${APP_DIR}/failed"
if [[ "${SVC_USER}" != "root" ]]; then
  chown -R "${SVC_USER}" "${APP_DIR}/logs" "${APP_DIR}/out" \
    "${APP_DIR}/inbox" "${APP_DIR}/outbox" "${APP_DIR}/processed" "${APP_DIR}/failed" 2>/dev/null || true
fi

# write_unit <name> <description> <exec-args>
write_unit() {
  cat >"/etc/systemd/system/$1.service" <<UNIT
[Unit]
Description=$2
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
User=${SVC_USER}
WorkingDirectory=${APP_DIR}
Environment=BSO_PORT=${PORT}
ExecStart=${RSCRIPT} $3
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
UNIT
}

write_unit bankstatements "Bank Statement OCR web app" "${APP_DIR}/scripts/run_app.R"
systemctl daemon-reload
systemctl enable --now bankstatements
echo "==> Installed 'bankstatements' (web app), enabled at boot and started."

if [[ "${1:-}" == "--inbox" ]]; then
  # One long-lived poller under Restart=always -- no overlapping runs, no cron.
  write_unit bankstatements-inbox "Bank Statement OCR inbox poller" \
    "${APP_DIR}/scripts/serve_inbox.R loop"
  systemctl daemon-reload
  systemctl enable --now bankstatements-inbox
  echo "==> Installed 'bankstatements-inbox' (folder poller), enabled at boot and started."
fi

IP="$(hostname -I 2>/dev/null | awk '{print $1}')"; IP="${IP:-<this-vm>}"
echo
echo "======================================================================"
echo " Auto-start is on. The app returns by itself after any reboot."
echo " Open:   http://${IP}:${PORT}"
echo " Status: systemctl status bankstatements"
echo " Logs:   journalctl -u bankstatements -f"
echo "======================================================================"
