from PIL import ImageTk, Image


def load_image(image_path, size):
    return ImageTk.PhotoImage(Image.open(image_path).resize(size, Image.ADAPTIVE))
