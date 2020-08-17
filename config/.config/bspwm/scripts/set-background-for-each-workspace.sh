bgdir="/home/user/.local/share/bg"
while read a m ws; do
  feh --no-fehbg --bg-fill "$bgdir/$(bspc query -D -d $ws --names).png"
done < <(bspc subscribe desktop_focus)
