import os
import re
from shutil import copy

from jinja2 import Environment, PackageLoader, select_autoescape

# let's just be stupid for now while i figure out a better setup
tl_substitutions = {
    "index.jinja": {
        "competitions": {
            "mens_t20_world_cup_2021": "Men's T20 World Cup 2021",
            "mens_t20_world_cup_2022": "Men's T20 World Cup 2022",
        },
    }
}

TEMPLATE_DIR = "competitions"
SITE_DIR = "competitions-site"
if not os.path.exists(SITE_DIR):
    os.makedirs(SITE_DIR)

jinja_file_regex = re.compile(r'\.jinja$')

env = Environment(
    loader=PackageLoader(TEMPLATE_DIR, "."),
    autoescape=select_autoescape()
)
for file, subs in tl_substitutions.items():
    template = env.get_template(file)
    file = jinja_file_regex.sub(".html", file)
    new_full_file = os.path.join(SITE_DIR, file)
    with open(new_full_file, "w+", encoding="utf8") as f:
        f.write(template.render(**subs))


# base_templates = ["base.jinja"]
#
# for root, dirs, files in os.walk(TEMPLATE_DIR):
#     path = root.split(os.sep)
#     path.append(".")
#     subdir = os.path.join(*path[1:])
#
#     new_root = root.replace(TEMPLATE_DIR, SITE_DIR)
#     for file in files:
#         new_full_file = os.path.join(new_root, file)
#         if not os.path.exists(new_root):
#             os.makedirs(new_root)
#         if jinja_file_regex.search(file):
#             if file not in base_templates:
#                 env = Environment(
#                     loader=PackageLoader(TEMPLATE_DIR, subdir),
#                     autoescape=select_autoescape()
#                 )
#                 template = env.get_template(file)
#                 new_full_file = jinja_file_regex.sub(".html", new_full_file)
#                 with open(new_full_file, "w+", encoding="utf8") as f:
#                     f.write(template.render())
#         else:
#             copy(os.path.join(root, file), new_full_file)

