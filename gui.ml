type coords = {
  x : int;
  y : int;
}

let window_size = " 1000x700"

let window_title = "Stuck In The Desert"

let load_png png_name = Png.load_as_rgb24 png_name []

let img_width img = fst (Images.size img)

let img_height img = snd (Images.size img)

let draw_at_coords img loc = Graphics.draw_image img loc.x loc.y

let update_player new_img old_img new_loc old_loc =
  Graphics.draw_image old_img old_loc.x old_loc.y;
  draw_at_coords new_img new_loc
