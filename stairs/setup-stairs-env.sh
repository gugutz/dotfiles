echo "## install Toggl Time Tracking"
# either the `toggldesktop` or `toggldesktop-bin` worked when i first tried
# installing the `toggldesktop-dev-bin` version for now
# yay -S --noconfirm toggldesktop
yay -S --noconfirm toggldesktop-dev-bin

echo "## install Jira Desktop app"
yay -S --noconfirm atlassian-jira

echo "## install Github Desktop app"
# installing the `-bin` package on recomentation from the package manager that also owns `github-desktop` on aur
# he says compiling the main package is painfull, so he recommends the `-bin` version
yay -S --noconfirm github-desktop-bin
