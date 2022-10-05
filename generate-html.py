import os
import re
from shutil import copy

from jinja2 import Environment, PackageLoader, select_autoescape

TEMPLATE_DIR = "competitions"
SITE_DIR = "competitions-site"
if not os.path.exists(SITE_DIR):
    os.makedirs(SITE_DIR)

env = Environment(
    loader=PackageLoader("competitions", "mens_t20_world_cup_2021"),
    autoescape=select_autoescape()
)

template = env.get_template("mens_t20_world_cup_2021.jinja")
# print(template.render(rule="some text"))

jinja_file_regex = re.compile(r'\.jinja$')

for root, dirs, files in os.walk(TEMPLATE_DIR):
    path = root.split(os.sep)
    path.append(".")
    subdir = os.path.join(*path[1:])
    # print(os.path.basename(root))
    new_root = root.replace(TEMPLATE_DIR, SITE_DIR)
    for file in files:
        new_full_file = os.path.join(new_root, file)
        if not os.path.exists(new_root):
            os.makedirs(new_root)
        if jinja_file_regex.search(file):
            env = Environment(
                loader=PackageLoader(TEMPLATE_DIR, subdir),
                autoescape=select_autoescape()
            )
            template = env.get_template(file)
            new_full_file = jinja_file_regex.sub(".html", new_full_file)
            with open(new_full_file, "w+", encoding="utf8") as f:
                f.write(template.render())
        else:
            copy(os.path.join(root, file), new_full_file)
        
        print(new_full_file)
