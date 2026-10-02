#!/usr/bin/env python3
"""Specimens for the redaction-overlay guard: a box over live text, four colours,
plus the shaded-header case that must NOT be flagged.

WHY THESE EXIST. detect_occluded_words (R/detect_redaction.R) used to ask only "is
this word DARK", so a WHITE box over live text -- one of the commonest failed
redactions there is -- hid an account number completely and was missed. Measured
here: black caught, white/grey/yellow missed. It now also asks whether the word can
still be SEEN, which a flat patch of any colour cannot be.

And the case that keeps that honest: a SHADED TABLE HEADER is a filled rectangle
over its own column names, drawn behind them. Calling that a redaction would
withhold the header of every statement that shades one.

Run: python3 tests/testthat/fixtures/make_overlay_fixtures.py
Writes: redaction_overlay_colours.pdf, redaction_shaded_header.pdf
Needs Python 3.9+ and reportlab. Dev-time only; the server runs R alone.
"""
import os
import sys

try:
    from reportlab.pdfgen import canvas
    from reportlab.lib.pagesizes import A4
except ImportError as e:
    sys.exit("needs reportlab (%s): python3 -m pip install reportlab" % e)

W, H = A4
OUT = os.path.dirname(os.path.abspath(__file__))


def colours(path):
    """The same account number covered four ways. All four hide it completely."""
    c = canvas.Canvas(path, pagesize=(W, H))
    c.setFont("Helvetica", 9)
    y = H - 100
    for label, rgb in [("BLACK", (0, 0, 0)), ("WHITE", (1, 1, 1)),
                       ("GREY", (0.88, 0.88, 0.88)), ("YELLOW", (1, 1, 0.3))]:
        txt = "%s  01-9988-004321%d-00" % (label, 7 + len(label) % 4)
        c.setFillColorRGB(0, 0, 0)
        c.drawString(40, y, txt)                                  # live in the text layer
        wd = c.stringWidth(txt, "Helvetica", 9)
        c.setFillColorRGB(*rgb)
        c.rect(38, y - 3, wd + 4, 13, stroke=0, fill=1)           # fully covering it
        y -= 40
    c.setFillColorRGB(0, 0, 0)
    c.drawString(40, y, "CLEAR  01-9988-0099999-00")              # the control
    c.save()


def shaded(path):
    """A shaded header BEHIND its text, and one genuine white-box redaction below."""
    c = canvas.Canvas(path, pagesize=(W, H))
    y = H - 100
    c.setFillColorRGB(0.85, 0.85, 0.85)
    c.rect(38, y - 3, 400, 13, stroke=0, fill=1)                  # drawn FIRST = behind
    c.setFillColorRGB(0, 0, 0)
    c.setFont("Helvetica-Bold", 9)
    c.drawString(40, y, "Date      Transaction type and details      Withdrawals   Balance")
    c.setFont("Helvetica", 9)
    y -= 25
    c.drawString(40, y, "03 Feb    EFTPOS RIVERSIDE DAIRY                 263.05   1,996.10")
    y -= 40
    txt = "ACCOUNT 01-9988-0043217-00"
    c.drawString(40, y, txt)
    wd = c.stringWidth(txt, "Helvetica", 9)
    c.setFillColorRGB(1, 1, 1)
    c.rect(38, y - 3, wd + 4, 13, stroke=0, fill=1)               # drawn AFTER = on top
    c.save()


if __name__ == "__main__":
    colours(os.path.join(OUT, "redaction_overlay_colours.pdf"))
    shaded(os.path.join(OUT, "redaction_shaded_header.pdf"))
    print("wrote redaction_overlay_colours.pdf and redaction_shaded_header.pdf")
