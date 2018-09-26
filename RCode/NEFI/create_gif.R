
create_gif <- function(gif_file, frame_delay, task_names=NULL) {
  
  # run imageMagick convert to build a gif
  if(is.null(task_names)) task_names <- '*'
  png_files <- paste(sprintf('Figs/NEFI/tmp/time_%s.png', task_names), collapse=' ')
  magick_command <- sprintf('magick convert -delay %d -loop 0 %s %s',
                            frame_delay, png_files, gif_file)
  system(magick_command)
  
  # simplify the gif with gifsicle - cuts size by about 2/3
  gifsicle_command <- sprintf('gifsicle -b -O3 %s --colors 256', gif_file)
  system(gifsicle_command)
}

frame_delay_cs = 8 # delay between frames of the gif animation. cs = centiseconds = hundredths of a second

task_names = seq(1, length(forecast_times)-1)

gif_file = 'Figs/NEFI/out/forecast_5.gif'

create_gif(gif_file = gif_file, frame_delay = frame_delay_cs, task_names = task_names)


####
frame_delay_cs = 200 # delay between frames of the gif animation. cs = centiseconds = hundredths of a second

task_names = c(16, 19)

gif_file = 'Figs/NEFI/out/forecast_ic.gif'

create_gif(gif_file = gif_file, frame_delay = frame_delay_cs, task_names = task_names)
