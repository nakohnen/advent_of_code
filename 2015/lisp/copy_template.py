import os
import shutil
import sys

def create_day(day_code):
    src = "template"
    dest = f"day{day_code.lower()}"

    if os.path.exists(dest):
        print(f"Directory {dest} already exists. Aborting.")
        return

    # Step 1: Create the destination folder
    os.makedirs(dest)

    # Step 2: Copy input.txt (only this file is copied)
    shutil.copy2(os.path.join(src, "input.txt"), os.path.join(dest, "input.txt"))

    # Step 3: Create symlinks for launch.lisp
    os.symlink(os.path.abspath(os.path.join(src, "launch.lisp")),
               os.path.join(dest, "launch.lisp"))

    # Step 4: Copy main.lisp aoc-day.asd and replace TEMPLATE_XYZ
    for filename in ("main.lisp", "aoc-day.asd"):
        main_src = os.path.join(src, filename)
        main_dest = os.path.join(dest, filename)

        with open(main_src, "r", encoding="utf-8") as f:
            content = f.read()

        content = content.replace("TEMPLATE_XYZ", day_code.upper())

        with open(main_dest, "w", encoding="utf-8") as f:
            f.write(content)

    print(f"Created {dest} successfully with symlinks and updated main.lisp!")

# 🔥 Entry point
if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python copy_template.py <day_code>")
    else:
        create_day(sys.argv[1])

