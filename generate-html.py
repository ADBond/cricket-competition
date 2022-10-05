import os

from jinja2 import Environment, PackageLoader, select_autoescape

SITE_DIR = "competitions-site"
if not os.path.exists(SITE_DIR):
    os.makedirs(SITE_DIR)

env = Environment(
    loader=PackageLoader("competitions", "mens_t20_world_cup_2021"),
    autoescape=select_autoescape()
)

template = env.get_template("mens_t20_world_cup_2021.jinja")
print(template.render(rule="some text"))

