import tkinter as tk  # Importing the Tkinter library for GUI development
import os  # Importing the os library for interacting with the file system
from tkinter import font  # Importing font module to work with fonts in Tkinter
from config import (  # Importing color configurations from the config file
    TOP_BAR_COLOR,
    SIDE_MENU_COLOR,
    MAIN_BODY_COLOR,
    MENU_CURSOR_HOVER_COLOR,
)
import utils.window_utils as window_utils  # Import custom utilities for window management
import utils.image_utils as util_image  # Import custom utilities for image handling
from PIL import ImageFont  # Import ImageFont from PIL to use custom fonts
from interface.text_editor import (
    TextEditor,
)  # Import the TextEditor class from text_editor.py


class GUI(tk.Tk):  # Define the main GUI class, inheriting from Tkinter's Tk class
    def __init__(self):
        super().__init__()  # Initialize the parent Tk class

        # Path to the custom font file
        self.custom_font_path = (
            "./src/interface/assets/fonts/ttf/JetBrainsMono-Regular.ttf"
        )

        # Load the logo image from the specified path with given dimensions
        self.logo = util_image.load_image(
            "./src/interface/assets/images/logo.png", (500, 136)
        )
        # Load the profile image from the specified path with given dimensions
        self.profile = util_image.load_image(
            "./src/interface/assets/images/logo.png", (100, 100)
        )

        # Index for text animation
        self.index = 0
        # List of strings to display in the animation (a typing effect simulation)
        self.txt = [
            "R|",
            "RI|",
            "RIC|",
            "RICA|",
            "RICAR|",
            "RICARD|",
            "RICARDI|",
            "RICARDIO|",
            "RICARDIOU|",
            "RICARDIOUS|",
        ]

        self.setup()  # Call the method to configure window setup
        self.setup_top_bar()  # Setup the top bar UI elements
        self.top_bar_controls()  # Add controls to the top bar
        self.side_menu_controls()  # Setup side menu controls

    # Method to create a custom font by loading it from a file with the specified size
    def create_custom_font(self, size):
        custom_font = ImageFont.truetype(
            self.custom_font_path, size=size
        )  # Load the font
        return font.Font(
            family=custom_font.getname()[0], size=size
        )  # Return the custom font

    # Setup basic window properties such as title and icon
    def setup(self):
        self.title("RICARDIOUS")  # Set the window title
        self.iconbitmap("./src/interface/assets/images/logo.ico")  # Set the window icon
        w, h = 1024, 600  # Define the window width and height
        window_utils.center_window(self, w, h)  # Center the window on the screen

    # Setup the top bar, side menu, and main body layout
    def setup_top_bar(self):
        # Create a frame for the top bar with the specified background color and height
        self.top_bar = tk.Frame(self, bg=TOP_BAR_COLOR, height=50)
        self.top_bar.pack(side=tk.TOP, fill="both")  # Position the top bar at the top

        # Create a frame for the side menu with the specified width and background color
        self.side_menu = tk.Frame(self, bg=SIDE_MENU_COLOR, width=150)
        self.side_menu.pack(
            side=tk.LEFT, fill="both", expand=False
        )  # Position the side menu on the left

        # Create a frame for the main body section with the specified background color
        self.main_body = tk.Frame(self, bg=MAIN_BODY_COLOR)
        self.main_body.pack(
            side=tk.RIGHT, fill="both", expand=True
        )  # Position the main body on the right

    # Add various controls (like buttons and labels) to the top bar
    def top_bar_controls(self):
        font_awesome = font.Font(
            family="FontAwesome", size=15
        )  # Create a FontAwesome font

        # Create a label for the title animation and configure its appearance
        self.lblTitle = tk.Label(self.top_bar, text="|")
        self.lblTitle.config(
            fg="white",
            font=self.create_custom_font(15),  # Use the custom font
            bg=TOP_BAR_COLOR,
            pady=10,
            width=16,
        )
        self.lblTitle.pack(side=tk.LEFT)  # Pack the label to the left side
        self.after(1000, self.start_animation)  # Start the animation after 1 second

        # Create a button for toggling the side menu (represented by a FontAwesome icon)
        self.sidebarMenuButton = tk.Button(
            self.top_bar,
            text="\uf0c9",  # FontAwesome icon for a menu
            command=self.toggle_menu,  # Assign the function to toggle the side menu
            font=font_awesome,
            bg=TOP_BAR_COLOR,
            fg="white",
            bd=0,
        )
        self.sidebarMenuButton.pack(side=tk.LEFT)  # Position the button on the left

        # Create a label for the user profile name and configure its appearance
        self.lblTitleUser = tk.Label(self.top_bar, text="alex._.cast")
        self.lblTitleUser.config(
            fg="white",
            font=self.create_custom_font(12),  # Use the custom font
            bg=TOP_BAR_COLOR,
            padx=10,
            width=20,
        )
        self.lblTitleUser.pack(side=tk.RIGHT)  # Position the user label on the right

    # Start the animation for the title text (simulates typing effect)
    def start_animation(self):
        if not self.index + 1 > len(self.txt):  # Check if the animation should continue
            self.lblTitle.config(text=self.txt[self.index])  # Update the label text
            self.index += 1  # Move to the next character
            self.after(
                1000, self.start_animation
            )  # Continue the animation every second
        else:
            self.index = 0  # Reset the animation
            self.lblTitle.config(text="|")  # Reset the text
            self.after(
                1000, self.start_animation
            )  # Restart the animation after a second

    # Setup the side menu controls like buttons and profile picture
    def side_menu_controls(self):
        menu_width = 20  # Define the width for the menu buttons
        menu_height = 2  # Define the height for the menu buttons

        font_awesome = font.Font(
            family="FontAwesome", size=20
        )  # Create a FontAwesome font
        custom_font = self.create_custom_font(
            14
        )  # Create a custom font for the buttons

        # Add a profile image to the top of the side menu
        self.labelProfile = tk.Label(
            self.side_menu, image=self.profile, bg=SIDE_MENU_COLOR
        )
        self.labelProfile.pack(
            side=tk.TOP, pady=10
        )  # Pack the profile image at the top

        # Define buttons to display in the side menu with text and icons
        buttons_info = [
            ("Text Editor", "\uf1cb", self.open_text_editor),
            ("About", "\uf007", self.open_text_editor),
            ("Exit", "\uf2f5", self.open_text_editor),
        ]

        self.buttons = []  # Create a list to hold the buttons
        for (
            text,
            icon,
            command,
        ) in buttons_info:  # Loop through button info and create each button
            button = tk.Button(self.side_menu)
            self.configure_button(
                button,
                text,
                icon,
                font_awesome,
                custom_font,
                menu_width,
                menu_height,
                command,
            )
            self.bind_hover_events(button)  # Add hover effects to the button
            self.buttons.append(button)  # Add the button to the list

    # Configure each button's appearance and text
    def configure_button(
        self,
        button,
        text,
        icon,
        font_awesome,
        custom_font,
        menu_width,
        menu_height,
        command,
    ):
        button.config(
            text=f"   {icon}   {text}",  # Format button text with the icon
            anchor="w",  # Align the text to the left
            font=custom_font,  # Use the custom font
            bg=SIDE_MENU_COLOR,  # Set the background color
            fg="white",  # Set the text color
            bd=0,  # Remove border
            width=menu_width,  # Set button width
            height=menu_height,  # Set button height
            command=command,  # Assign the button command
        )
        button.pack(side=tk.TOP)  # Pack the button to the top of the side menu

    # Bind hover events to the buttons to change their appearance on hover
    def bind_hover_events(self, button):
        button.bind("<Enter>", lambda event: self.on_enter(event, button))  # On hover
        button.bind("<Leave>", lambda event: self.on_leave(event, button))  # On leave

    # Change button appearance when hovered over
    def on_enter(self, event, button):
        button.config(bg=MENU_CURSOR_HOVER_COLOR, fg="white")  # Change background color

    # Revert button appearance when hover is removed
    def on_leave(self, event, button):
        button.config(bg=SIDE_MENU_COLOR, fg="white")  # Revert background color

    # Toggle the visibility of the side menu
    def toggle_menu(self):
        if (
            self.side_menu.winfo_ismapped()
        ):  # Check if the side menu is currently visible
            self.side_menu.pack_forget()  # Hide the side menu
        else:
            self.side_menu.pack(
                side=tk.LEFT, fill="both", expand=False
            )  # Show the side menu

    def open_text_editor(self):
        TextEditor(self.main_body)  # Open the text editor window
