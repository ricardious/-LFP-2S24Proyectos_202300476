import tkinter as tk
from tkinter import filedialog, messagebox
import os


class RoundedButton(tk.Canvas):
    def __init__(
        self,
        parent,
        width,
        height,
        cornerradius,
        text,
        command=None,
        start_color="#0078d7",
        end_color="#005bb5",
        **kwargs,
    ):
        tk.Canvas.__init__(
            self,
            parent,
            height=height,
            width=width,
            bg=parent["bg"],
            bd=0,
            highlightthickness=0,
        )
        self.command = command
        self.start_color = start_color
        self.end_color = end_color

        rad = 2 * cornerradius

        # Draw gradient background
        self.draw_gradient(width, height, start_color, end_color)

        # Draw rounded rectangle
        self.create_arc(
            (0, 0, rad, rad), start=90, extent=90, fill=start_color, outline=""
        )
        self.create_arc(
            (width - rad, 0, width, rad),
            start=0,
            extent=90,
            fill=start_color,
            outline="",
        )
        self.create_arc(
            (width - rad, height - rad, width, height),
            start=270,
            extent=90,
            fill=end_color,
            outline="",
        )
        self.create_arc(
            (0, height - rad, rad, height),
            start=180,
            extent=90,
            fill=end_color,
            outline="",
        )
        self.create_rectangle(
            (cornerradius, 0, width - cornerradius, height),
            fill=start_color,
            outline="",
        )
        self.create_rectangle(
            (0, cornerradius, width, height - cornerradius),
            fill=start_color,
            outline="",
        )
        # Create text on the button
        self.create_text(
            width / 2,
            height / 2,
            text=text,
            fill=kwargs.get("fg", "white"),
            font=kwargs.get("font", ("Helvetica", 12)),
        )

        # Bind the click event to the command
        if command:
            self.bind("<ButtonPress-1>", lambda event: self.on_click())

    def draw_gradient(self, width, height, start_color, end_color):
        """Draw a vertical gradient from start_color to end_color."""
        r1, g1, b1 = self.winfo_rgb(start_color)
        r2, g2, b2 = self.winfo_rgb(end_color)
        r_ratio = (r2 - r1) / height
        g_ratio = (g2 - g1) / height
        b_ratio = (b2 - b1) / height

        for i in range(height):
            nr = int(r1 + (r_ratio * i))
            ng = int(g1 + (g_ratio * i))
            nb = int(b1 + (b_ratio * i))
            color = f"#{nr // 256:02x}{ng // 256:02x}{nb // 256:02x}"
            self.create_line(0, i, width, i, fill=color)

    def on_click(self):
        if self.command:
            self.command()


class TextEditor:
    def __init__(self, main_frame):
        # Configure grid layout
        self.main_frame = main_frame
        self.main_frame.grid_rowconfigure(0, weight=1)
        self.main_frame.grid_columnconfigure(0, weight=2)
        self.main_frame.grid_columnconfigure(1, weight=3)

        # Text editor on the left side
        self.text_frame = tk.Frame(self.main_frame, bg="white")
        self.text_frame.grid(row=0, column=0, padx=10, pady=10, sticky="nsew")
        self.text_frame.grid_rowconfigure(1, weight=1)
        self.text_frame.grid_columnconfigure(0, weight=1)

        # Button frame at the top of the text editor
        self.button_frame_top = tk.Frame(self.text_frame, bg="white")
        self.button_frame_top.grid(row=0, column=0, sticky="ew", pady=5)

        # Add rounded buttons (Open, Save, Save As) at the top
        open_button = RoundedButton(
            self.button_frame_top,
            width=120,
            height=40,
            cornerradius=10,
            text="Open",
            start_color="#0078d7",
            end_color="#005bb5",
            fg="white",
            font=("Helvetica", 12),
            command=self.open_file,
        )
        open_button.pack(side="left", padx=5)

        save_button = RoundedButton(
            self.button_frame_top,
            width=120,
            height=40,
            cornerradius=10,
            text="Save",
            start_color="#28a745",
            end_color="#19692c",
            fg="white",
            font=("Helvetica", 12),
            command=self.save_file,
        )
        save_button.pack(side="left", padx=5)

        save_as_button = RoundedButton(
            self.button_frame_top,
            width=120,
            height=40,
            cornerradius=10,
            text="Save As",
            start_color="#ffc107",
            end_color="#ff8f00",
            fg="black",
            font=("Helvetica", 12),
            command=self.save_as_file,
        )
        save_as_button.pack(side="left", padx=5)

        # Text editor widget
        self.text_editor = tk.Text(
            self.text_frame,
            wrap="word",
            bg="white",
            fg="black",
            font=("Helvetica", 12),
            height=15,
            width=50,
        )
        self.text_editor.grid(row=1, column=0, padx=5, pady=5, sticky="nsew")

        # Analyze button at the bottom of the text editor
        analyze_button = RoundedButton(
            self.text_frame,
            width=120,
            height=40,
            cornerradius=10,
            text="Analyze",
            start_color="#dc3545",
            end_color="#c82333",
            fg="white",
            font=("Helvetica", 12),
            command=self.analyze_text,
        )
        analyze_button.grid(row=2, column=0, pady=10, sticky="s")

        # Right frame with graph and controls
        self.right_frame = tk.Frame(self.main_frame, bg="white")
        self.right_frame.grid(row=0, column=1, padx=10, pady=10, sticky="nsew")
        self.right_frame.grid_rowconfigure(2, weight=1)
        self.right_frame.grid_columnconfigure(0, weight=1)

        # Canvas for graph
        self.grafo_frame = tk.Frame(self.right_frame, bg="white", bd=2)
        self.grafo_frame.pack(expand=True, fill="both", padx=5, pady=5)
        self.canvas = tk.Canvas(self.grafo_frame, width=300, height=200, bg="white")
        self.canvas.pack()
        self.draw_sample_graph()

        # Generate PDF button at the bottom of the graph frame
        generate_pdf_button = RoundedButton(
            self.right_frame,
            width=120,
            height=40,
            cornerradius=10,
            text="Generate PDF",
            start_color="#0078d7",
            end_color="#005bb5",
            fg="white",
            font=("Helvetica", 12),
            command=self.generate_pdf,
        )
        generate_pdf_button.pack(side="bottom", pady=10)

        # Store the current file path
        self.current_file = None

    def open_file(self):
        file_path = filedialog.askopenfilename(
            filetypes=[("Text Files", "*.ORG"), ("All Files", "*.*")]
        )
        if file_path:
            with open(file_path, "r", encoding="utf-8") as file:
                content = file.read()
                self.text_editor.delete(1.0, tk.END)
                self.text_editor.insert(tk.END, content)
            self.current_file = file_path

    def save_file(self):
        if self.current_file:
            with open(self.current_file, "w", encoding="utf-8") as file:
                file.write(self.text_editor.get(1.0, tk.END))
        else:
            self.save_as_file()

    def save_as_file(self):
        file_path = filedialog.asksaveasfilename(
            defaultextension=".txt",
            filetypes=[("Text Files", "*.ORG"), ("All Files", "*.*")],
        )
        if file_path:
            with open(file_path, "w", encoding="utf-8") as file:
                file.write(self.text_editor.get(1.0, tk.END))
            self.current_file = file_path

    def analyze_text(self):
        content = self.text_editor.get(1.0, tk.END)
        if content.strip():
            print("Analyzing text...")  # Replace with actual analysis
        else:
            messagebox.showwarning("Analysis", "The editor is empty.")

    def draw_sample_graph(self):
        self.canvas.create_oval(120, 20, 180, 80, fill="yellow")
        self.canvas.create_text(150, 50, text="Name", font=("Helvetica", 12))
        self.canvas.create_line(150, 80, 100, 130)
        self.canvas.create_line(150, 80, 200, 130)
        self.canvas.create_oval(70, 130, 130, 190, fill="red")
        self.canvas.create_oval(170, 130, 230, 190, fill="green")
        self.canvas.create_text(100, 160, text="Country 1", font=("Helvetica", 12))
        self.canvas.create_text(200, 160, text="Country 2", font=("Helvetica", 12))

    def generate_pdf(self):
        print("Generating PDF...")


if __name__ == "__main__":
    root = tk.Tk()
    root.title("Text Editor with Gradient Buttons")
    root.geometry("800x600")
    root.configure(bg="white")
    editor = TextEditor(root)
    root.mainloop()
