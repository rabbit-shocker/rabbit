add_theme_path("cozmixng-images")
add_theme_path("rabbit-images")

@powered_by_images = ["powered-by-cozmixng.png", "rabbit-pink-logo.png"]
@powered_by_text = "Powered by COZMIXNG and Rabbit #{Rabbit::VERSION}"
include_theme("powered-by")
