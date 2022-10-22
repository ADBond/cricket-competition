import os
import re
from shutil import copy
from datetime import datetime

from yaml import full_load as yaml_load
# import yaml

from jinja2 import Environment, PackageLoader, select_autoescape

import pandas as pd
import numpy as np


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


def make_person_link(p_id):
    return f"person_{p_id}.html"
def make_team_link(code):
    return f"{code}.html"

link_format_lookup = {
    "person": make_person_link,
    "team": make_team_link,
}
def enlinken(names, ids, link_format):
    if link_format not in link_format_lookup.keys():
        raise ValueError(f"link format must be one of: {list(link_format_lookup.keys())}, not '{link_format}'")
    func = link_format_lookup[link_format]
    enlinkened_names = []
    for name, p_id in zip(names, ids):
        href = f"./{func(p_id)}"
        enlinkened_names.append(
            f"<a href='{href}'>{name}</a>"
        )
    return enlinkened_names

def get_info(df_group_tables, team):
    if team not in df_group_tables["display_name"].values:
        return None
    team_row = df_group_tables[df_group_tables["display_name"] == team]
    group = team_row["group"].unique()
    if len(group) > 1:
        raise ValueError(f"More than one team table found for team {team} ({group})")
    gp_tab = df_group_tables[df_group_tables["group"].isin(group)]
    gp_rename = {
        "display_name": "Team",
        "matches": "Matches played", "wins": "Wins", "losses": "Losses",
        "ties": "Ties", "nr": "No-results", "points": "Points", "nrr": "Net Run Rate",
        "runs_scored": "Runs scored", "overs_faced": "Overs faced",
        "runs_against": "Runs conceded", "overs_bowled": "Overs bowled",
        "overs_faced_eff": "Overs faced (effective)", 
        "overs_bowled_eff": "Overs bowled (effective)",
    }
    gp_tab = gp_tab[gp_rename.keys()]
    gp_tab = gp_tab.rename(columns=gp_rename)
    return {
        "table_ind": group[0],
        "table": gp_tab.to_html(index=False)
    }

STATIC_FOLDER = "site-static-files"
TEMPLATE_DIR = "competitions"
SITE_DIR = "competitions-site"
CURRENT_TIME = datetime.now().strftime("%A %d %B %Y, %H:%M:%S (%Z)")

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
sitewide_substitutions = {
    "date_time_update": CURRENT_TIME
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
        f.write(template.render(**subs, **sitewide_substitutions))

# TODO: maybe on team page instead of person + full list have person, num_shares
# TODO: and similarly on person page for teams
# TODO: results on team page even??
# TODO: page for ranking teams by share value!
for comp, subs in comp_substitutions.items():
    comp_template = env.get_template("competition_home.jinja")
    lb_template = env.get_template("leaderboard.jinja")
    file = f"{comp}.html"
    new_full_file = os.path.join(SITE_DIR, file)

    df_teams = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "teams.csv")).sort_values(by="display_name")
    df_participants = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "participants.csv")).sort_values(
        by=["display_name", "id"]
    )
    df_people_points_tot = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "generated", "participant-scores.csv"))
    df_people_points = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "generated", "participant-scores-by-share.csv"))
    
    df_group_tables = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "generated", "group-tables.csv")).merge(
        df_teams, how="left", left_on="team", right_on="code"
    )

    teams = dict(zip(df_teams["code"], df_teams["display_name"]))
    participants = df_participants.to_dict(orient="records")

    with open(new_full_file, "w+", encoding="utf8") as f:
        f.write(comp_template.render(comp=comp, teams=teams, **subs, **sitewide_substitutions))

    comp_folder = os.path.join(SITE_DIR, comp)
    if not os.path.exists(comp_folder):
        os.makedirs(comp_folder)
    
    participant_league_table_with_links = df_people_points_tot.merge(
            df_participants, how="left", left_on="participant_id", right_on="id"
        ).rename(
            columns={"display_name": "name"}
        )
    participant_league_table_with_links["Total points"] = np.round(participant_league_table_with_links["total_points"], 4)
    participant_league_table_with_links["Name"] = enlinken(
        participant_league_table_with_links["name"],
        participant_league_table_with_links["id"],
        "person"
    )
    participant_league_table_with_links = participant_league_table_with_links[["Name", "Total points"]]
    lb_subs = {
        "title": f"Leaderboard - {subs['title']}",
        "participant_league_table_with_links": participant_league_table_with_links.to_html(index=False, escape=False)
    }

    with open(os.path.join(SITE_DIR, comp, f"leaderboard.html"), "w+", encoding="utf8") as f:
        f.write(lb_template.render(**lb_subs, **sitewide_substitutions))
    
    team_template = env.get_template("team_page.jinja")
    df_points = pd.read_csv(os.path.join(TEMPLATE_DIR, comp, "data", "generated", "team-points-breakdown.csv"))
    for code, team in teams.items():
        df_team_points = df_points[df_points["team"] == team]
        total_points = df_team_points["points"].sum()
        sum_row = pd.DataFrame(
            [{"team": "", "event": "<strong>total</total>", "points": f"<strong>{total_points}</strong>"}]
        )
        df_team_points = pd.concat(
            [
                df_team_points,
                sum_row,
            ],
            ignore_index=True
        )

        df_team_shares = df_people_points[df_people_points["team_code"] == code]
        total_shares = df_team_shares["total_shares"].unique()[0]
        share_value = df_team_shares["points_per_share"].unique()[0]
        df_team_shares = df_team_shares.merge(
            df_participants, how="left", left_on="participant_id", right_on="id",
            suffixes=["_team", "_person"]
        )
        df_team_shares = df_team_shares.sort_values(["display_name_person", "participant_id"])
        df_team_shares["person"] = enlinken(
            df_team_shares["display_name_person"], df_team_shares["participant_id"], "person"
        )
        df_team_shares = df_team_shares[["person"]]
        team_file = os.path.join(SITE_DIR, comp, make_team_link(code))

        stages = df_group_tables["stage"].unique()
        team_tables = {
            stage: info
            for stage in stages if (info := get_info(df_group_tables[df_group_tables["stage"] == stage], team)) is not None
        }

        with open(team_file, "w+", encoding="utf8") as f:
            f.write(team_template.render(
                team_points_table = df_team_points.to_html(index=False, escape=False),
                team_shares_table = df_team_shares.to_html(index=False, escape=False),
                total = total_points,
                total_shares = total_shares,
                share_value = share_value,
                team_tables = team_tables,
                title = f"{team} - {subs['title']}",
                comp_home = file,
                **sitewide_substitutions,
            ))

    person_template = env.get_template("person_page.jinja")
    for participant in participants:
        p_id = participant["id"]
        p_name = participant["display_name"]
        # TODO: eliminated flag for each team???
        df_person_points = df_people_points[df_people_points["participant_id"] == p_id]
        df_person_points = df_person_points.sort_values(["points_per_share", "team_code"], ascending=[False, True])
        df_person_points["Team"] = enlinken(
            df_person_points["display_name"], df_person_points["team_code"], "team"
        )
        df_person_points = df_person_points[["Team", "matches_played", "points_per_share"]]
        total_points = df_person_points["points_per_share"].sum()
        sum_row = pd.DataFrame(
            [{"Team": "<strong>total</total>", "matches_played": "", "points_per_share": f"<strong>{total_points}</strong>"}]
        )
        df_person_points = pd.concat([df_person_points, sum_row], ignore_index=True)
        df_person_points = df_person_points.rename(
            columns={"matches_played": "Matches played", "points_per_share": "Points/share"}
        )
        person_file = os.path.join(SITE_DIR, comp, make_person_link(p_id))
        with open(person_file, "w+", encoding="utf8") as f:
            f.write(person_template.render(
                person_points_table = df_person_points.to_html(index=False, escape=False),
                title = f"{p_name} - {subs['title']}",
                comp_home = file,
                **sitewide_substitutions,
            ))

for file in os.listdir(STATIC_FOLDER):
    copy(os.path.join(STATIC_FOLDER, file), os.path.join(new_static_folder, file))
