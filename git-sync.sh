#!/bin/bash
git pull --no-edit
wget -N http://ssb22.user.srcf.net/mwrhome/jianpu-ly.py

(
    awk -- 'BEGIN {p=1} /^Run jianpu-ly / {p=0} // {if(p) print}' < README.md
    python jianpu-ly.py --markdown | awk -- 'BEGIN {p=0} /^Run jianpu-ly / {p=1} // {if(p) print}'
cat <<EOF

Copyright and Trademarks
------------------------

(c) Silas S. Brown, licensed under the GPL.

Python is a trademark of the Python Software Foundation.
Any other trademarks I mentioned without realising are trademarks of their respective holders.
EOF
    ) > n && mv n README.md

git commit -am update && git push
