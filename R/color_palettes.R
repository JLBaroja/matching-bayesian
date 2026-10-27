team_colors <- c(
  ARI = "#97233F",  # Cardinals Red
  ATL = "#A71930",  # Falcons Red
  BAL = "#241773",  # Ravens Purple
  BUF = "#00338D",  # Bills Royal Blue
  CAR = "#0085CA",  # Panthers Blue
  CHI = "#0B162A",  # Bears Navy Blue
  CIN = "#FB4F14",  # Bengals Orange
  CLE = "#311D00",  # Browns Brown
  DAL = "#041E42",  # Cowboys Navy (different from CHI)
  DEN = "#FB4F14",  # Broncos Orange (adjusted below)
  DET = "#0076B6",  # Lions Honolulu Blue
  GB  = "#203731",  # Packers Dark Green
  HOU = "#03202F",  # Texans Deep Steel Blue
  IND = "#002C5F",  # Colts Royal Blue (different from BUF)
  JAX = "#006778",  # Jaguars Teal
  KC  = "#E31837",  # Chiefs Red (brighter than ATL)
  LA  = "#003594",  # Rams Royal Blue (adjusted from BUF/IND)
  LAC = "#0080C6",  # Chargers Powder Blue
  LV  = "#000000",  # Raiders Black
  MIA = "#008E97",  # Dolphins Aqua
  MIN = "#4F2683",  # Vikings Purple (warmer than BAL)
  NE  = "#002244",  # Patriots Navy (unique from CHI/DAL)
  NO  = "#D3BC8D",  # Saints Old Gold
  NYG = "#0B2265",  # Giants Blue
  NYJ = "#125740",  # Jets Green
  PHI = "#004C54",  # Eagles Midnight Green
  PIT = "#101820",  # Steelers Black (slightly lighter than LV)
  SEA = "#002244",  # Seahawks Navy (same as NE — optionally tweak)
  SF  = "#AA0000",  # 49ers Red
  TB  = "#D50A0A",  # Buccaneers Red (deeper crimson)
  TEN = "#4B92DB",  # Titans Blue
  WAS = "#5A1414"   # Commanders Burgundy
)
quarter_colors <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e')
year_colors <- c('#40004b','#762a83','#9970ab','#c2a5cf','#e7d4e8','#d9f0d3','#a6dba0','#5aae61','#1b7837','#00441b') 
year_colors <- c(
  "#bfd3e6",
  "#adc7df",
  "#9ebcda",
  "#92aed2",
  "#8c96c6",
  "#8c7ab9",
  "#8c6bb1",
  "#8946a3",
  "#88419d",
  "#810f7c"
)
bird_colors <- c("#3182bd","#d95f02","#e6ab02", "#1b9e77", "#66a61e", "#7570b3")
elo_colors <- c('#4575b4','#91bfdb','#e9a3c9','#c51b7d')
elo_colors <- c('#0007ff','#00bfff','#00ff80','#00ff00')
elo_colors <- color_ch(elo_colors,pctg=-0.25)
elo_colors <- c("#D9534F", "#F0AD4E", "#5BC0DE", "#428BCA")
elo_colors <- c('#fe9929',
'#fec44f',
'#41b6c4',
'#2c7fb8')
bird_labels <- paste('Bird',1:6)
elo_low <- c(961,1440,1585,1722)
elo_high <- c(1439,1584,1721,2546)
elo_labels <- paste('ELO between\n',elo_low,' and ',elo_high,sep='')
year_labels <- as.character(2014:2023)
