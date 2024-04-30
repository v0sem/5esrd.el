import json
from iteration_utilities import unique_everseen

with open("data/11 monsters.json") as f:
    monsters = json.load(f)

files = {"Unaligned": []}

for dict in monsters["Monsters"]:
    if "Monsters" in dict:
        thing = monsters["Monsters"][dict]
        for key, value in thing.items():
            if "content" in value:
                print(key)
                files["Unaligned"].append({key: value})
            else:
                files[key] = []
                for k, v in value.items():
                    files[key].append({k: v})

for key, mlist in files.items():
    with open(f"monsters/{key}.json", "w") as f:
        for m in mlist:
            f.write(str(m) + "\n")
