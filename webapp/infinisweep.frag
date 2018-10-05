precision mediump float;
uniform vec2 u_windowSize;
const int grid_size = 25;
float cell_size = min(u_windowSize.x, u_windowSize.y)/float(grid_size);

void draw_grid(vec4 color) {
  for (int x = 0; x < grid_size; x++) {
    if ((gl_FragCoord.x > float(x) * u_windowSize.x / float(grid_size) - 2.5 &&
        gl_FragCoord.x < float(x) * u_windowSize.x / float(grid_size) + 2.5) ||
        (gl_FragCoord.y > float(x) * u_windowSize.y / float(grid_size) - 2.5 &&
        gl_FragCoord.y < float(x) * u_windowSize.y / float(grid_size) + 2.5))
      gl_FragColor = vec4(color);
  };
}

// draw a 0 at the given coordinate
void draw_0(ivec2 coord) {
  vec2 screen_coord = vec2(coord) * u_windowSize / float(grid_size);
  if (length(screen_coord - gl_FragCoord.xy + vec2(cell_size/2.0)) < 10.0) {
    gl_FragColor = vec4(0.6,0.6,1.0,1.0);
  }
}

void main() {
  // grey background
  gl_FragColor = vec4(0.95,0.95,0.9,1.0);

  draw_grid(vec4(0.2,0.5,0.8,1.0));

  draw_0(ivec2(2,2));
}
