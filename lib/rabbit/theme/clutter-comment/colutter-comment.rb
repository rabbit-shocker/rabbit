theme_exit unless display?

proc_name = "clutter-comment"

canvas.delete_on_comment_proc_by_name(proc_name)

theme_exit if @clutter_comment_uninstall

canvas.on_comment(proc_name) do |comment|
  stage = canvas.renderer.widget.stage

  color = Clutter::Color.new(rand(256), rand(256), rand(256),
                             128 + rand(128))
  size = 36 + rand(72)
  actor = Clutter::Label.new("Sans #{size}", comment, color)
  actor.x = canvas.width
  actor.y = (canvas.height / 2.0) * rand
  actor.show
  stage.add(actor)

  n = 150 + rand(150)
  delta = (canvas.width + actor.width) / n.to_f

  do_rotate_x = rand(5).zero?
  do_rotate_y = rand(5).zero?
  do_rotate_z = rand(5).zero?
  do_scale = rand(5).zero?

  x_axis = actor.x
  y_axis = actor.y - (actor.height * 0.5)
  if do_rotate_x
    actor.set_rotation(Clutter::X_AXIS, rand(360), 0, y_axis, 0)
  end
  if do_rotate_y
    actor.set_rotation(Clutter::Y_AXIS, rand(360), x_axis, 0, 0)
  end
  if do_rotate_z
    actor.set_rotation(Clutter::Z_AXIS, rand(360), x_axis, y_axis, 0)
  end
  if do_scale
    x_sign = 1
    y_sign = 1
    actor.set_scale(-2 + rand(5), -2 + rand(5))
  end
  GLib::Timeout.add(1000 / 50) do
    actor.raise_top
    actor.x -= delta

    x_angle, x_x, x_y, x_z = actor.get_rotation(Clutter::X_AXIS)
    y_angle, y_x, y_y, y_z = actor.get_rotation(Clutter::Y_AXIS)
    z_angle, z_x, z_y, z_z = actor.get_rotation(Clutter::Z_AXIS)
    x_angle = (x_angle + rand(5)) % 360
    y_angle = (y_angle + rand(5)) % 360
    z_angle = (z_angle + rand(5)) % 360
    x_axis = actor.x
    if do_rotate_x
      actor.set_rotation(Clutter::X_AXIS, x_angle, 0, y_axis, 0)
    end
    if do_rotate_y
      actor.set_rotation(Clutter::Y_AXIS, y_angle, x_axis, 0, 0)
    end
    if do_rotate_z
      actor.set_rotation(Clutter::Z_AXIS, z_angle, x_axis, y_axis, 0)
    end
    if do_scale
      x_scale, y_scale = actor.scale
      x_sign *= -1 if x_scale.abs > 2
      y_sign *= -1 if y_scale.abs > 2
      actor.set_scale(x_scale + (0.1 * x_sign),
                      y_scale + (0.1 * y_sign))
    end
    n -= 1
    stage.remove(actor) if n.zero?
    not n.zero?
  end
end
