def center_window(window, widht, height):
    screen_width = window.winfo_screenwidth()
    screen_height = window.winfo_screenheight()
    x = int((screen_width / 2) - (widht / 2))
    y = int((screen_height / 2) - (height / 2))
    return window.geometry(f"{widht}x{height}+{x}+{y}")
