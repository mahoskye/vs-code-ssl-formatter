#!/usr/bin/env python3
"""Generate the VS Code icon from media/ssl-formatter.svg shapes."""
from __future__ import annotations

from pathlib import Path
from typing import Iterable, List, Tuple

from PIL import Image, ImageDraw

SIZE = 256
RENDER_SCALE = 4  # render larger then downsample for cleaner edges
CANVAS = SIZE * RENDER_SCALE
SVG_VIEWBOX = 200  # svg uses 0 0 200 200
SCALE = CANVAS / SVG_VIEWBOX
OUTPUT_PATH = Path(__file__).resolve().parents[1] / "media" / "icon.png"

Color = Tuple[int, int, int, int]
Point = Tuple[float, float]


def scale_point(point: Point) -> Point:
    return point[0] * SCALE, point[1] * SCALE


def quad_curve(start: Point, ctrl: Point, end: Point, steps: int = 12) -> List[Point]:
    """Sample a quadratic curve into a list of points (including the end)."""
    pts: List[Point] = []
    for i in range(1, steps + 1):
        t = i / steps
        one_minus = 1 - t
        x = one_minus * one_minus * start[0] + 2 * one_minus * t * ctrl[0] + t * t * end[0]
        y = one_minus * one_minus * start[1] + 2 * one_minus * t * ctrl[1] + t * t * end[1]
        pts.append((x, y))
    return pts


def add_path(draw: ImageDraw.ImageDraw, commands: Iterable[Tuple[str, Tuple[Point, ...]]], fill: Color, outline: Color, width: int = 1):
    """Render a simple path consisting of move/line/quad commands."""
    points: List[Point] = []
    current: Point | None = None
    for cmd, args in commands:
        if cmd == "M":
            current = args[0]
            points.append(current)
        elif cmd == "L":
            current = args[0]
            points.append(current)
        elif cmd == "Q":
            assert current is not None
            ctrl, end = args
            points.extend(quad_curve(current, ctrl, end))
            current = end
        else:
            raise ValueError(f"Unsupported command {cmd}")
    scaled = [scale_point(p) for p in points]
    draw.polygon(scaled, fill=fill, outline=outline, width=width)


def main():
    image = Image.new("RGBA", (CANVAS, CANVAS), (0, 0, 0, 0))
    draw = ImageDraw.Draw(image)

    # Background
    draw.rectangle((0, 0, CANVAS, CANVAS), fill="#0b1020")

    # Star
    star_points = [
        (100, 15),
        (121, 70),
        (180, 75),
        (133, 112),
        (147, 170),
        (100, 140),
        (53, 170),
        (67, 112),
        (20, 75),
        (79, 70),
    ]
    draw.polygon([scale_point(p) for p in star_points], fill="#f6c945", outline="#e0a800", width=int(3 * RENDER_SCALE))

    # Flask body (approximated quadratics)
    body_commands = [
        ("M", ((85, 40),)),
        ("L", ((115, 40),)),
        ("L", ((110, 80),)),
        ("L", ((135, 150),)),
        ("Q", ((137, 155), (132, 160))),
        ("L", ((68, 160),)),
        ("Q", ((63, 155), (65, 150))),
        ("L", ((90, 80),)),
    ]
    add_path(
        draw,
        body_commands,
        fill=(245, 247, 255, int(0.6 * 255)),
        outline=(31, 41, 51, 255),
        width=int(3 * RENDER_SCALE),
    )

    # Flask neck inner
    neck_rect = [scale_point((90, 40)), scale_point((110, 48))]
    draw.rectangle((*neck_rect[0], *neck_rect[1]), fill=(255, 255, 255, int(0.8 * 255)))

    # Liquid inside the flask
    liquid_commands = [
        ("M", ((72, 138),)),
        ("Q", ((90, 132), (100, 134))),
        ("Q", ((110, 136), (128, 130))),
        ("L", ((133, 150),)),
        ("Q", ((135, 155), (130, 158))),
        ("L", ((70, 158),)),
        ("Q", ((65, 155), (67, 150))),
    ]
    add_path(
        draw,
        liquid_commands,
        fill=(69, 180, 255, int(0.85 * 255)),
        outline=(27, 75, 107, 255),
        width=int(2 * RENDER_SCALE),
    )

    # Bubbles
    def bubble(center: Point, radius: float):
        cx, cy = scale_point(center)
        r = radius * SCALE
        draw.ellipse((cx - r, cy - r, cx + r, cy + r), fill=(230, 244, 255, 255))

    bubble((92, 144), 3)
    bubble((108, 140), 2.5)
    bubble((100, 150), 2)

    # Small shine on the flask
    shine_points = [(90, 85), (84, 105), (88, 125)]
    shine_scaled = [scale_point(p) for p in shine_points]
    draw.line(shine_scaled, fill=(255, 255, 255, int(0.7 * 255)), width=int(2 * RENDER_SCALE), joint="curve")

    # Downsample to final size
    final_image = image.resize((SIZE, SIZE), resample=Image.Resampling.LANCZOS)
    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    final_image.save(OUTPUT_PATH)
    print(f"Wrote icon to {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
