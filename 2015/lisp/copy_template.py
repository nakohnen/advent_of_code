import os
import shutil

def create_day(day_code):
    src = "template"
    dest = f"day{day_code.lower()}"
    
    if os.path.exists(dest):
        print(f"Directory {dest} already exists. Aborting.")
        return
    
    # Step 1: Copy the template folder
    shutil.copytree(src, dest)
    
    # Step 2: Replace placeholder TEMPLATE_XYZ in aoc-day.asd (or other files)
    replacements = {
        "TEMPLATE_XYZ": day_code.upper(),  # Replace TEMPLATE_XYZ with DAY CODE (e.g., 02B)
    }
    
    for root, _, files in os.walk(dest):
        for filename in files:
            filepath = os.path.join(root, filename)
            with open(filepath, "r+", encoding="utf-8") as f:
                content = f.read()
                for old, new in replacements.items():
                    content = content.replace(old, new)
                f.seek(0)
                f.write(content)
                f.truncate()
    
    print(f"Created {dest} successfully!")

# Example usage
if __name__ == "__main__":
    for x in range(24):
        for sub_x in ("a", "b"):
            create_day(f"{x+1:02}{sub_x}")

