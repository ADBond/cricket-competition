import os
import re
from shutil import copy

from yaml import full_load as yaml_load
# import yaml

from jinja2 import Environment, PackageLoader, select_autoescape

import pandas as pd


# super hard-code for now
def subs_from_yaml(yaml_file):
    with open(yaml_file, "r") as f:
        config = yaml_load(f.read())
    points_obj = config["points"]

    results = points_obj["match-result-points"]
    multipliers = points_obj["stage-multipliers"]
    mr_bonuses = points_obj["match-result-bonuses"]
    bonuses = points_obj["bonus-points"]
    return {
        "rule_result_points": {
            "A win": results["win"],
            "A tie": results["tie"],
            "A no-result": results["no_result"],
            "A win on a tie-break": results["tie_win"],
            "A loss on a tie-break": results["tie_lose"],
            "A loss [although see below]": results["lose"],
        },
        "rule_multipliers": {
            "In the first round": multipliers["first_round"],
            "In the super-12s": multipliers["super_12"],
            "In the semi-finals": multipliers["semi_finals"],
            "In the final": multipliers["final"],
        },
        "rule_fixed_points": {
            "Close loss (losing with NRR less than 1 - i.e. less than 20 runs in a match going to 20 overs)": mr_bonuses["close_loss"],
            "Bowling out opposing team": mr_bonuses["bowled_out_opponent"],
            "Not conceding any wickets": mr_bonuses["no_wickets_lost"],
            "Making it from the first round to super 12s": bonuses["to_super_12"],
            "Making it to the knockout stages": bonuses["to_knockouts"],
            "Member of team taking a hat-trick": bonuses["hat_trick"],
            "Member of team making a century": bonuses["century"],
            "Member of team taking a five-for": bonuses["fifer"],
        }
    }


STATIC_FOLDER = "site-static-files"
TEMPLATE_DIR = "competitions"
SITE_DIR = "competitions-site"
if not os.path.exists(SITE_DIR):
    os.makedirs(SITE_DIR)
new_static_folder = os.path.join(SITE_DIR, "static")
if not os.path.exists(new_static_folder):
    os.makedirs(new_static_folder)

# let's just be stupid for now while i figure out a better setup
tl_substitutions = {
    "index.jinja": {
        "competitions": {
            "mens_t20_world_cup_2021": "Men's T20 World Cup 2021",
            "mens_t20_world_cup_2022": "Men's T20 World Cup 2022",
        },
    },
}
comp_substitutions = {
    "mens_t20_world_cup_2021": {
        "title": "Men's T20 World Cup 2021",
        **subs_from_yaml(os.path.join(TEMPLATE_DIR, "mens_t20_world_cup_2021", "points-allocation.yaml")),
    },
    "mens_t20_world_cup_2022": {
        "title": "Men's T20 World Cup 2022",
        **subs_from_yaml(os.path.join(TEMPLATE_DIR, "mens_t20_world_cup_2022", "points-allocation.yaml"))
    },
}

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

for comp, subs in comp_substitutions.items():
    comp_template = env.get_template("competition_home.jinja")
    file = f"{comp}.html"
    new_full_file = os.path.join(SITE_DIR, file)

    df_teams = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "teams.csv"))
    df_participants = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "participants.csv"))
    teams = dict(zip(df_teams["code"], df_teams["display_name"]))
    # print(teams)
    participants = df_participants.to_dict(orient="records")

    with open(new_full_file, "w+", encoding="utf8") as f:
        f.write(comp_template.render(comp=comp, teams=teams, **subs))

    comp_folder = os.path.join(SITE_DIR, comp)
    if not os.path.exists(comp_folder):
        os.makedirs(comp_folder)
    team_template = env.get_template("team_page.jinja")
    df_points = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "generated", "team-points-breakdown.csv"))
    for code, team in teams.items():
        df_team_points = df_points[df_points["team"] == team]
        team_file = os.path.join(SITE_DIR, comp, f"{code}.html")
        with open(team_file, "w+", encoding="utf8") as f:
            f.write(team_template.render(
                team_points_table = df_team_points.to_html(index=False), title = f"{team} - {subs['title']}"
            ))

    person_template = env.get_template("person_page.jinja")
    df_people_points = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "generated", "participant-scores-by-share.csv"))
    for participant in participants:
        p_id = participant["id"]
        p_name = participant["display_name"]
        df_person_points = df_people_points[df_people_points["participant_id"] == p_id]
        person_file = os.path.join(SITE_DIR, comp, f"person_{p_id}.html")
        with open(person_file, "w+", encoding="utf8") as f:
            f.write(person_template.render(
                person_points_table = df_person_points.to_html(index=False), title = f"{p_name} - {subs['title']}"
            ))

for file in os.listdir(STATIC_FOLDER):
    copy(os.path.join(STATIC_FOLDER, file), os.path.join(new_static_folder, file))

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

