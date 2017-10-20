lsof -i tcp:5570 | awk 'NR!=1 {print $2}' | xargs kill

