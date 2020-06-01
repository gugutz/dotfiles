This is quite a complicated conky setup. A minimal knowledge of conky/lua is required.
It will not align the same way on two different screens/resolutions/distros. Never. Some alignment is necessary.
Requirements: conky-all (with lua and cairo bindings), curl and inxi must be installed for it to work.

Included files:
- conkyrc : this is the main conky, and does all but the disk bar graph.
- conkyrc2 : calls bargraph.lua. lua_draw_hook_post does not work for my system, so I had to use two separate conkys to call two separate luas.
- graph.lua : draws the CPU and network line graphs. By wlourf - http://u-scripts.blogspot.com/2010/10/graph-widget.html.
- grid2.png : the grids for the above graphs.
- bargraph.lua : draws the disk I/O bar graph.
- kk.sh : little script that a) kills all conkys, b) starts both of these. It assumes these files are in ~/.conky. Edit if necessary.
You may want to make another one just like it but with some 12 or more seconds sleep for your startup one. It gives the network interface time to come up and display your external IP address.

Set-up:
Just put the graph.lua and grid2.png file in the same directory as your conkyrc (or edit PATHs accordingly).
If you don't like the grid, just remove the file or the conky entry.
Adjust gap_y and/or alignment to fit your screen.
This is for a 4-core CPU. If you have more or less... edit conkyrc and graph.lua accordingly.
Replace enp3s0 in conkyrc to eth0, wlan0, or whatever network interface (4 instances) and in graph.lua (2 instances).
Replace /dev/sdb1 for the partition you want to display disk I/O for in bargraph.lua.
Uncomment line 93 in conkyrc - and change sda2 for the partition you want to display - for a disk use bar.
If you have nvidia-settings, uncomment lines 94 and 95 in conkyrc for a gpu usage bar.

Under KDE: (line 59), check the output of inxi -c 0 -Sx in a terminal and adjust accordingly.
Temperature: (line 65), hwmon 1 temp 4 works on this machine. On others it could be hwmon 0 temp 1 or some other combination. 

Editing conkyrc will reload it, but kill graph.lua. Use kk.sh when all looks correct to reload all.
Any doubts, just ask.

