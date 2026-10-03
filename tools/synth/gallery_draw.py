#!/usr/bin/env python3
"""gallery_draw.py -- one picture per case: the page(s) with the columns the reader picked
drawn over them, and a panel saying what it decided, why, and what is wrong.
   python3 tools/synth/gallery_draw.py <json_dir> <out_dir>"""
import json, os, subprocess, sys, tempfile, textwrap
from PIL import Image, ImageDraw, ImageFont

DPI = 80
COL = {"date": (31, 119, 180), "date2": (23, 190, 207), "description": (127, 127, 127),
       "debit": (214, 39, 40), "credit": (44, 160, 44), "amount": (148, 103, 189),
       "balance": (255, 127, 14), "other": (140, 86, 75)}

def font(sz):
    for f in ("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", "/usr/share/fonts/dejavu/DejaVuSans.ttf"):
        if os.path.exists(f):
            return ImageFont.truetype(f, sz)
    return ImageFont.load_default()

def render(pdf, page):
    d = tempfile.mkdtemp()
    subprocess.run(["pdftoppm", "-png", "-r", str(DPI), "-f", str(page), "-l", str(page), pdf, d + "/p"], check=True)
    f = sorted(os.listdir(d))[0]
    return Image.open(os.path.join(d, f)).convert("RGB")

def colour(field):
    for k, v in COL.items():
        if field == k or field.startswith(k):
            return v
    return COL["description"] if field.startswith("text") else COL["other"]

def panel_text(r):
    out = [f"{r['case']}   [{r['set']}, {r['kind']}]",
           f"Outcome: {r['outcome'].upper()}   figures right {r['right']} of {r['want_n']} (read {r['got_n']})",
           "Why: " + (r.get("why") or "")]
    fc = r.get("failed_checks") or []
    if fc:
        out.append("Checks failed: " + "; ".join(f"{c['check']}" for c in fc))
    for title, key in (("Missing / wrong (answer key)", "missing"), ("Extra / wrong (read)", "extra")):
        rows = r.get(key) or []
        if rows:
            out.append(f"{title}: " + "; ".join(
                f"{x.get('date')} {x.get('amount')} {str(x.get('description',''))[:28]}" for x in rows[:4])
                + (" ..." if len(rows) > 4 else ""))
    return out

def main(jdir, odir):
    os.makedirs(odir, exist_ok=True)
    for jf in sorted(os.listdir(jdir)):
        if not jf.endswith(".json"):
            continue
        r = json.load(open(os.path.join(jdir, jf)))
        path = r["path"]
        cols = r.get("columns") or []
        pages = sorted({c["page"] for c in cols}) or [1]
        focus = pages[:2]
        if path.endswith(".pdf"):
            imgs = []
            for p in focus:
                try:
                    im = render(path, p)
                except Exception:
                    continue
                dr = ImageDraw.Draw(im, "RGBA")
                s = DPI / 72.0
                for c in cols:
                    if c["page"] != p:
                        continue
                    rgb = colour(str(c["field"]))
                    x0, x1 = c["x_min"] * s, c["x_max"] * s
                    dr.rectangle([x0, 0, x1, im.height], fill=rgb + (38,), outline=rgb + (200,), width=2)
                    dr.rectangle([x0, 2, x0 + 7 * len(str(c["field"])) + 6, 18], fill=rgb + (230,))
                    dr.text((x0 + 3, 3), str(c["field"]), fill=(255, 255, 255), font=font(11))
                imgs.append(im)
            if not imgs:
                imgs = [Image.new("RGB", (600, 200), "white")]
        else:
            imgs = [Image.new("RGB", (900, 120), "white")]
            ImageDraw.Draw(imgs[0]).text((10, 40), f"{os.path.basename(path)} (export: no page image)", fill="black", font=font(16))
        w = sum(i.width for i in imgs) + 10 * (len(imgs) - 1)
        h = max(i.height for i in imgs)
        lines = []
        for t in panel_text(r):
            lines += textwrap.wrap(t, width=max(60, w // 8)) or [""]
        ph = 22 * len(lines) + 20
        canvas = Image.new("RGB", (max(w, 900), h + ph), "white")
        x = 0
        for im in imgs:
            canvas.paste(im, (x, ph)); x += im.width + 10
        d = ImageDraw.Draw(canvas)
        d.rectangle([0, 0, canvas.width, ph - 6], fill=(245, 245, 245))
        for i, t in enumerate(lines):
            d.text((10, 10 + 22 * i), t, fill=(0, 0, 0), font=font(15))
        canvas.save(os.path.join(odir, jf.replace(".json", ".png")))
        print("drew", jf)

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
