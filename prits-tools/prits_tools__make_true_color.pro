FUNCTION prits_tools::make_true_color, image, rr, gg, bb
  compile_opt static
  r=rr[image]
  g=gg[image]
  b=bb[image]
  true_image = [[[r]], [[g]], [[b]]]
  return, true_image
END

