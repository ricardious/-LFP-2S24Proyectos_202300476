import tkinter as tk
from tkinter import Canvas
from tkinter import font


# Funciones utilitarias
def HEXcov(r, g, b):
    return f"#{r:02x}{g:02x}{b:02x}"  # Modificado para usar 2 dígitos por canal


def draw_gradient(
    canvas: Canvas, start_color: str, end_color: str, direction: str = "horizontal"
):
    canvas.update()

    start_color_rgb = canvas.winfo_rgb(start_color)
    end_color_rgb = canvas.winfo_rgb(end_color)

    width = canvas.winfo_width()
    height = canvas.winfo_height()

    if direction == "horizontal":
        for i in range(width):
            r = (
                int(
                    start_color_rgb[0]
                    + (end_color_rgb[0] - start_color_rgb[0]) * i / width
                )
                >> 8
            )
            g = (
                int(
                    start_color_rgb[1]
                    + (end_color_rgb[1] - start_color_rgb[1]) * i / width
                )
                >> 8
            )
            b = (
                int(
                    start_color_rgb[2]
                    + (end_color_rgb[2] - start_color_rgb[2]) * i / width
                )
                >> 8
            )
            color = HEXcov(r, g, b)
            canvas.create_line(i, 0, i, height, fill=color)
    elif direction == "vertical":
        for i in range(height):
            r = (
                int(
                    start_color_rgb[0]
                    + (end_color_rgb[0] - start_color_rgb[0]) * i / height
                )
                >> 8
            )
            g = (
                int(
                    start_color_rgb[1]
                    + (end_color_rgb[1] - start_color_rgb[1]) * i / height
                )
                >> 8
            )
            b = (
                int(
                    start_color_rgb[2]
                    + (end_color_rgb[2] - start_color_rgb[2]) * i / height
                )
                >> 8
            )
            color = HEXcov(r, g, b)
            canvas.create_line(0, i, width, i, fill=color)
