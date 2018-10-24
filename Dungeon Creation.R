if(!("png" %in% installed.packages())){install.packages("png");library("png")}else{library("png")}

no <- '
create_dungeon <- function(x_size = 150, y_size = 150, number_of_rooms = 7, size_of_rooms = c("small", "medium", "large"), seed = 1){
  
}



x_size <- 600
y_size <- 600
number_of_rooms <- 7
size_of_rooms <- "small"
seed <- 1
pic <- array(0, dim=c(x_size, y_size, 3))
#for(x in 1:x_size){
#  spot <- sample(1, x = c(1,2,3,4,5,6,7))
#  if(spot == 4){spot = c(1,2)} else if (spot == 5){spot = c(1,3)} else if (spot == 6){spot = c(2, 3)}else if(spot == 7){spot = c(1,2,3)}
#  for(y in 1:y_size){
#    pic[x,y,spot] <- runif(1)
#  }
#}
writePNG(image = pic, target = "tested.png")
if(!("png" %in% installed.packages())){install.packages("png");library("png")}else{library("png")}
pic <- array(0, dim=c(x_size, y_size, 3))
room_cores <- list()
room <- 1
while(length(room_cores)<=number_of_rooms){
  x_spot <- sample(1, x = 30:(x_size-30))
  y_spot <- sample(1, x = 30:(y_size-30))
  print(paste(x_spot, y_spot, sep = " : "))
  if(is_too_close(min_dist = 120, theory_point = c(x_spot, y_spot), core_list = room_cores)){ 
    room_cores[[room]] <- c(x_spot, y_spot)
    room <- room+1
  }
}
for(x in room_cores){
  print(x)
  if(size_of_rooms == "medium"){
    if(x[1]<70){
      x_drop <- sample(1, x=20:(x[1]-1))
    } else {x_drop <- sample(1, x=20:70)}
    if((x_size -x[1])<70){
      x_add <- sample(1, x=20:(x_size-x[1]-1))
    } else {x_add <- sample(1, x=20:70)}
    if(x[2]<70){
      y_drop <- sample(1, x=20:(x[2]-1))
    } else {y_drop <- sample(1, x=20:70)}
    if((y_size-x[2])<70){
      y_add <- sample(1, x=20:(y_size-x[2]-1))
    } else {y_add <- sample(1, x=20:70)}
  }
  print(paste(x_drop, x_add, y_drop, y_add))
  pic[(x[1]-x_drop):(x[1]+x_add),(x[2]-y_drop):(x[2]+y_add),] <- 1
}
writePNG(image = pic, target = "tested.png")





is_too_close <- function(min_dist = 100, theory_point, core_list){
  if(length(core_list)==0){print("Length was 0");return(TRUE)}
  
  for(x in core_list){
    if(sqrt((x[1]-theory_point[1])^2+(x[2]-theory_point[2])^2)<=min_dist){print(paste("failed on", x, "with", theory_point));return(FALSE)}
  }
  print("Returned True")
  return(TRUE)
}



for(current_iteration in 1:1000){
  width <- sample(1, x = 20:74)*2+1
  height <- sample(1, x = 20:74)*2+1
  pic <- array(0, dim = c(width+2, height+2, 3))
  pic[,,3] <- 1
  pic[2:(width+1), 2:(height+1), ] <- 1
  writePNG(image = pic, target = paste(current_iteration, ".png", sep = ""))
}











#Begin Generation
#up - 1
#right - 2
#down - 3
#left - 4
'



blank <- 'not_full <- TRUE
paths_exist <- TRUE
room_cores <- list() #List of two
room_dims <- list() #List of two
paths <- list() # List of 4 (x, y, direction, Done yet)
temperature <- 2
room_num <- 1
fullx <- 900
fully <- 900
is_first_room <- TRUE
startingx <- sample(1, x=(fullx/3):(fullx*2/3))
startingy <- sample(1, x=(fully/3):(fully*2/3))
while(not_full && paths_exist){
  pic_num <- sample(1, x = 1:1000)
  current_room <- readPNG(source = paste(pic_num, "png", sep = "."))
  c_height <- length(current_room[1,,1])
  c_width <- length(current_room[,1,1])
  room_center <- c((c_width-1)/2+1, (c_height-1)/2+1)
  if(is_first_room){
    room_cores[[room_num]] <- room_center+c(startingx, startingy)
    is_first_room <- FALSE
  } else { room_cores[[room_num]] <- room_center}
  room_dims <- c(c_width, c_height)
  
  
  
  
  #Paths algorithm here
    
  room_num <- room_num+1  
}'









#Real attempt

map_x <- 1000
map_y <- 1000
map <- array(0, dim = c(map_x, map_y, 3))
path_size <- c(10, 10)
room_cores <- list()  # <y, x>
room_edges <- list()  # <top_left (y,x) bot_right(y,x)
paths <- list()       # start (y,x) stop (y,x) direction (1, 2, 3, 4)

room_x_dims <- c(30, 50)
room_y_dims <- c(30, 50)

first_room <- c(sample(1, x = (map_y/3):(map_y*2/3)), sample(1, x = (map_x/3):(map_x*2/3)))
room_cores[[1]] <- first_room
x_drop <- as.integer(sample(1, x=(room_x_dims[1]):(room_x_dims[2]))/2)
x_add  <- as.integer(sample(1, x=(room_x_dims[1]):(room_x_dims[2]))/2)

y_drop <- as.integer(sample(1, x=(room_y_dims[1]):(room_y_dims[2]))/2)
y_add  <- as.integer(sample(1, x=(room_y_dims[1]):(room_y_dims[2]))/2)

room_edges[[1]] <- c(first_room[1]-x_drop, first_room[2]-y_drop, first_room[1]+x_add, first_room[2]+y_add)

map[room_edges[[1]][1]:room_edges[[1]][3], room_edges[[1]][2]:room_edges[[1]][4], ] <- 1
writePNG(map, target = "Generate.png")
path_extensions <- sample(3, x = c(1,2,3,4))   # 1 = right; 2 = down; 3 = left; 4 = up
for(x in path_extensions){
  if(x == 1){
    x_size <- room_edges[[1]][3]-room_edges[[1]][1]
    x_spot <- sample(1, x=(as.integer(room_edges[[1]][1]+x_size/4)):(as.integer(room_edges[[1]][3]-x_size/4)))
    y_spot <- room_edges[[1]][4]
    paths[[length(paths)+1]] <- c(x_spot, y_spot, x_spot, y_spot+ sample(1, x=path_size[1]:path_size[2]), 1)
  }
  if(x == 2){
    y_size <- room_edges[[1]][4]-room_edges[[1]][2]
    x_spot <- room_edges[[1]][3]
    y_spot <- sample(1, x=(as.integer(room_edges[[1]][2]+y_size/4)):(as.integer(room_edges[[1]][4]-y_size/4)))
    paths[[length(paths)+1]] <- c(x_spot, y_spot, x_spot+ sample(1, x=path_size[1]:path_size[2]), y_spot, 2)
  }
  if(x==3){
    x_size <- room_edges[[1]][3]-room_edges[[1]][1]
    x_spot<- sample(1, x=(as.integer(room_edges[[1]][1]+x_size/4)):(as.integer(room_edges[[1]][3]-x_size/4)))
    y_spot <- room_edges[[1]][2]
    paths[[length(paths)+1]] <- c(x_spot, y_spot, x_spot, y_spot- sample(1, x=path_size[1]:path_size[2]), 3)
  }
  if(x==4){
    y_size <- room_edges[[1]][4]-room_edges[[1]][2]
    x_spot <- room_edges[[1]][1]
    y_spot <- sample(1, x=(as.integer(room_edges[[1]][2]+y_size/4)):(as.integer(room_edges[[1]][4]-y_size/4)))
    paths[[length(paths)+1]] <- c(x_spot, y_spot, x_spot- sample(1, x=path_size[1]:path_size[2]), y_spot, 4)
  }
  
}


writePNG(map, target = "Generate_paths.png")

wanted_rooms <- 150
current_path <- 1
removeable_paths <- c()
Found_inter <- FALSE
used_paths <- list()
new_paths_per_room <- 2.5
for(room_num in 2:wanted_rooms){
  removeable_paths <- c()
  p_num <- 0
  Useable_Path <- FALSE
  while(p_num <= length(paths) && Useable_Path == FALSE){
    p_num <- p_num + 1
    if(p_num>length(paths)){print("Not Enough Paths");break;}
    max_up <- 0
    max_down <- 0
    max_left <- 0
    max_right <- 0
    runnin_up_max <- 9999
    runnin_down_max <- 9999
    runnin_left_max <- 9999
    runnin_right_max <- 9999
    #FOR UP PATH
    if(paths[[p_num]][5]==4){#Up Path
      cmax_left <- 0
      cmax_right <- 0
      hit_room <- FALSE
      pos <- paths[[p_num]][3]-1
      #Check up
      while(hit_room==FALSE && pos > 1 && max_up <= room_y_dims[2]){
        if(map[pos-1 ,paths[[p_num]][4], 1]==1){
          hit_room <- TRUE
        }else{
          pos <- pos-1
          max_up <- max_up + 1
          #Checks Left
          hit_side <- FALSE
          c_pos <- paths[[p_num]][4]
          cmax_left <- 0
          while(hit_side==FALSE && c_pos>1 && cmax_left <= room_x_dims[2]-room_x_dims[1]){
            if(map[pos, c_pos-1, 1]==1){
              hit_side <- TRUE
            } else {
              #map[pos, c_pos-1, c(2,3)] <- .5
              c_pos <- c_pos - 1
              cmax_left <- cmax_left + 1
            }
          }
          #Checks Right
          hit_side <- FALSE
          c_pos <- paths[[p_num]][4]
          cmax_right <- 0
          while(hit_side==FALSE&&c_pos < map_x-1 && cmax_right <= room_x_dims[2]-room_x_dims[1]){
            if(map[pos, c_pos+1, 1]==1){
              hit_side <- TRUE
            } else {
              #map[pos, c_pos+1, c(2,3)] <- .5
              c_pos <- c_pos + 1
              cmax_right <- cmax_right + 1
            }
          }
        }
        runnin_left_max <- min(runnin_left_max, cmax_left)
        runnin_right_max <- min(runnin_right_max, cmax_right)
      }
      max_left <- runnin_left_max
      max_right <- runnin_right_max
      max_down <- as.integer(max_up/2)
      max_up <- as.integer(max_up/2)
      if(max_up>=room_y_dims[1]/2&&max_down>=room_y_dims[1]/2&&max_left+max_right>=as.integer(room_x_dims[1])){
        Useable_Path <- TRUE
      } else {
        removeable_paths <- c(removeable_paths, p_num)
      }
    }
    if(paths[[p_num]][5]==2){#Down Path
      cmax_left <- 0
      cmax_right <- 0
      hit_room <- FALSE
      pos <- paths[[p_num]][3]+1
      #Checks Down
      while(hit_room==FALSE && pos < map_y-1 && max_down <= room_y_dims[2]){
        if(map[pos+1, paths[[p_num]][4], 1]==1){
          hit_room <- TRUE
        } else {
          pos <- pos + 1
          max_down <- max_down + 1
          #Checks Left
          hit_side <- FALSE
          c_pos <- paths[[p_num]][4]
          cmax_left <- 0
          while(hit_side==FALSE && c_pos>1 && cmax_left <= room_x_dims[2]-room_x_dims[1]){
            if(map[pos, c_pos-1, 1]==1){
              hit_side <- TRUE
            } else {
              #map[pos, c_pos-1, c(2,3)] <- .5
              c_pos <- c_pos - 1
              cmax_left <- cmax_left + 1
            }
          }
          #Checks Right
          hit_side <- FALSE
          c_pos <- paths[[p_num]][4]
          cmax_right <- 0
          while(hit_side==FALSE&&c_pos < map_x-1 && cmax_right <= room_x_dims[2]-room_x_dims[1]){
            if(map[pos, c_pos+1, 1]==1){
              hit_side <- TRUE
            } else {
              #map[pos, c_pos+1, c(2,3)] <- .5
              c_pos <- c_pos + 1
              cmax_right <- cmax_right + 1
            }
          }
        }
        runnin_left_max <- min(runnin_left_max, cmax_left)
        runnin_right_max <- min(runnin_right_max, cmax_right)
      }
      max_left <- runnin_left_max
      max_right <- runnin_right_max
      max_up <- as.integer(max_down/2)
      max_down <- as.integer(max_down/2)
      if(max_up>=room_y_dims[1]/2&&max_down>=room_y_dims[1]/2&&max_left+max_right>=as.integer(room_x_dims[1])){
        Useable_Path <- TRUE
      } else {
        removeable_paths <- c(removeable_paths, p_num)
      }
    }
    if(paths[[p_num]][5]==3){#Left Path
      cmax_up <- 0
      cmax_down <- 0
      hit_room <- FALSE
      pos <- paths[[p_num]][4]-1
      #Checks Left
      while(hit_room==FALSE && pos > 1 && max_left <= room_x_dims[2]){
        if(map[paths[[p_num]][3], pos-1, 1]==1){
          hit_room <- TRUE
        } else {
          pos <- pos - 1
          max_left <- max_left + 1
          #Checks up
          hit_side <- FALSE
          c_pos <- paths[[p_num]][3]
          cmax_up <- 0
          while(hit_side == FALSE && c_pos > 1 && cmax_up <= room_y_dims[2]-room_y_dims[1]){
            if(map[c_pos-1, pos, 1]==1){
              hit_side <- TRUE
            } else {
              #map[c_pos-1, pos, 2] <- .5
              c_pos <- c_pos - 1
              cmax_up <- cmax_up + 1
            }
          }
          #Checks down
          hit_side <- FALSE
          c_pos <- paths[[p_num]][3]
          cmax_down <- 0
          while(hit_side == FALSE && c_pos < map_y-1 && cmax_down <= room_y_dims[2]-room_y_dims[1]){
            if(map[c_pos + 1, pos, 1]==1){
              hit_side <- TRUE
            } else {
              #map[c_pos + 1, pos, 2] <- .5
              c_pos <- c_pos + 1
              cmax_down <- cmax_down + 1
            }
          }
        }
        runnin_up_max <- min(runnin_up_max, cmax_up)
        runnin_down_max <- min(runnin_down_max, cmax_down)
      }
      max_up <- runnin_up_max
      max_down <- runnin_down_max
      max_right <- as.integer(max_left/2)
      max_left <- as.integer(max_left/2)
      if(max_left>=room_x_dims[1]/2&&max_right>=room_x_dims[1]/2&&max_up+max_down >=as.integer(room_y_dims[1])){
        Useable_Path <- TRUE
      } else {
        removeable_paths <- c(removeable_paths, p_num)
      }
    }
    if(paths[[p_num]][5]==1){#Right Path
      cmax_up <- 0
      cmax_down <- 0
      hit_room <- FALSE
      pos <- paths[[p_num]][4] + 1
      #Checks right
      while(hit_room==FALSE && pos < map_x - 1 && max_right <= room_x_dims[2]){
        if(map[paths[[p_num]][3], pos+1, 1]==1){
          hit_room <- TRUE
        } else {
          pos <- pos + 1
          max_right <- max_right + 1
          #Checks up
          hit_side <- FALSE
          c_pos <- paths[[p_num]][3]
          cmax_up <- 0
          while(hit_side == FALSE && c_pos > 1 && cmax_up <= room_y_dims[2]-room_y_dims[1]){
            if(map[c_pos-1, pos, 1]==1){
              hit_side <- TRUE
            } else {
              #map[c_pos-1, pos, 2] <- .5
              c_pos <- c_pos - 1
              cmax_up <- cmax_up + 1
            }
          }
          #Checks down
          hit_side <- FALSE
          c_pos <- paths[[p_num]][3]
          cmax_down <- 0
          while(hit_side == FALSE && c_pos < map_y-1 && cmax_down <= room_y_dims[2]-room_y_dims[1]){
            if(map[c_pos + 1, pos, 1]==1){
              hit_side <- TRUE
            } else {
              #map[c_pos + 1, pos, 2] <- .5
              c_pos <- c_pos + 1
              cmax_down <- cmax_down + 1
            }
          }
        }
        runnin_up_max <- min(runnin_up_max, cmax_up)
        runnin_down_max <- min(runnin_down_max, cmax_down)
      }
      max_up <- runnin_up_max
      max_down <- runnin_down_max
      max_left <- as.integer(max_right/2)
      max_right <- as.integer(max_right/2)
      if(max_left>=room_x_dims[1]/2&&max_right>=room_y_dims/2&&max_up+max_down >=as.integer(room_y_dims[1])){
        Useable_Path <- TRUE
      } else {
        removeable_paths <- c(removeable_paths, p_num)
      }
    }
  }
  used_paths[[length(used_paths)+1]] <- paths[[p_num]]
  room_left <- sample(1, x=(as.integer(room_x_dims[1]/2+1)):(as.integer(min(max_left-1, room_x_dims[2]-1))))
  room_right <- sample(1, x=(as.integer(room_x_dims[1]/2+1)):(as.integer(min(max_right-1, room_x_dims[2]-1))))
  room_up <- sample(1, x=(as.integer(room_y_dims[1]/2+1)):(as.integer(min(max_up-1, room_y_dims[2]-1))))
  room_down <- sample(1, x=(as.integer(room_y_dims[1]/2+1)):(as.integer((min(max_up-1, room_y_dims[2]-1)))))
  if(paths[[p_num]][5]==1){
    room_cores[[length(room_cores)+1]]<-c(paths[[p_num]][3], paths[[p_num]][4]+room_left)
  } 
  if(paths[[p_num]][5]==2){
    room_cores[[length(room_cores)+1]]<-c(paths[[p_num]][3]+room_up, paths[[p_num]][4])
  } 
  if(paths[[p_num]][5]==3){
    room_cores[[length(room_cores)+1]]<-c(paths[[p_num]][3], paths[[p_num]][4]-room_right)
  } 
  if(paths[[p_num]][5]==4){
    room_cores[[length(room_cores)+1]]<-c(paths[[p_num]][3]-room_down, paths[[p_num]][4])
  }  
  room_edges[[length(room_edges)+1]]<-c(room_cores[[length(room_cores)]][1]-room_up+1,
                                        room_cores[[length(room_cores)]][2]-room_left+1,
                                        room_cores[[length(room_cores)]][1]+room_down-1,
                                        room_cores[[length(room_cores)]][2]+room_right-1)
  map[(room_edges[[length(room_edges)]][1]):(room_edges[[length(room_edges)]][3]), 
      (room_edges[[length(room_edges)]][2]):(room_edges[[length(room_edges)]][4]), ] <- 1
  removeable_paths <- c(removeable_paths, p_num)
  removeable_paths <- unique(removeable_paths)
  paths <- paths[!(1:length(paths) %in% removeable_paths)]
  
  #Generate new paths
  new_pths <- c()
  odds_of_new_paths <- new_paths_per_room
  while(odds_of_new_paths>0){
    if(runif(1)<odds_of_new_paths){
      new_pths <- c(new_pths, sample(1, x=c(1, 2, 3, 4)))
    }
    odds_of_new_paths <- odds_of_new_paths-1
  }
  for(x in new_pths){
    if(x == 1){
      x_size <- room_edges[[length(room_edges)]][3]-room_edges[[length(room_edges)]][1]
      x_spot <- sample(1, x=(as.integer(room_edges[[length(room_edges)]][1]+x_size/4)):(as.integer(room_edges[[length(room_edges)]][3]-x_size/4)))
      y_spot <- room_edges[[length(room_edges)]][4]
      paths[[length(paths)+1]] <- c(x_spot, y_spot, x_spot, y_spot+ sample(1, x=path_size[1]:path_size[2]), 1)
    }
    if(x == 2){
      y_size <- room_edges[[length(room_edges)]][4]-room_edges[[length(room_edges)]][2]
      x_spot <- room_edges[[length(room_edges)]][3]
      y_spot <- sample(1, x=(as.integer(room_edges[[length(room_edges)]][2]+y_size/4)):(as.integer(room_edges[[length(room_edges)]][4]-y_size/4)))
      paths[[length(paths)+1]] <- c(x_spot, y_spot, x_spot+ sample(1, x=path_size[1]:path_size[2]), y_spot, 2)
    }
    if(x==3){
      x_size <- room_edges[[length(room_edges)]][3]-room_edges[[length(room_edges)]][1]
      x_spot<- sample(1, x=(as.integer(room_edges[[length(room_edges)]][1]+x_size/4)):(as.integer(room_edges[[length(room_edges)]][3]-x_size/4)))
      y_spot <- room_edges[[length(room_edges)]][2]
      paths[[length(paths)+1]] <- c(x_spot, y_spot, x_spot, y_spot- sample(1, x=path_size[1]:path_size[2]), 3)
    }
    if(x==4){
      y_size <- room_edges[[length(room_edges)]][4]-room_edges[[length(room_edges)]][2]
      x_spot <- room_edges[[length(room_edges)]][1]
      y_spot <- sample(1, x=(as.integer(room_edges[[length(room_edges)]][2]+y_size/4)):(as.integer(room_edges[[length(room_edges)]][4]-y_size/4)))
      paths[[length(paths)+1]] <- c(x_spot, y_spot, x_spot- sample(1, x=path_size[1]:path_size[2]), y_spot, 4)
    }
    
  }
}

for(x in 1:length(used_paths)){
  map[(used_paths[[x]][1]-1):(used_paths[[x]][3]+1), (used_paths[[x]][2]-1):(used_paths[[x]][4]+1),c(1,3)] <- .5
  print("did")
}

writePNG(map, target = "Generate_first_room.png")






