open Graphics
let window = open_graph " 1000x700"; 
  set_window_title "This is the window title"; 
  let box = create_image 5 5 in
  draw_image box 500 500; 
  let c = read_key () in
  if c = 'w' then draw_image box 500 505 
  else if c = 's' then draw_image box 500 495 
  else if c = 'a' then draw_image box 495 500
  else if c = 'd' then draw_image box 505 500
  else close_graph ()
