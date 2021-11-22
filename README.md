# 3D Cellular Automaton in ASCII Art
### Main idea
The program records a cellular automaton as it evolves through a number of stages.
The automaton is represented as the list of 3d vectors corresponding to the alive cells in the automaton.
Then, the automaton is converted to a grid of characters that can be displayed in the terminal. 
This is done by rotating the CA around the Z (vertical) axis by an ever increasing angle, then around the X-axis by the angle that the point of view makes with the XY plane.
This causes an effect similar to a top-down view, where the x and y coordinates of the vectors can be correlated with x and y positions on the screen, and the z coordinate gives the depth which can be used in the coloring of pixels.
The projection mode is orthograthic, but a sense of perspective is induced by using a dimmer color for pixels that are further away from the point of view.

### Explanation of different files
- Recorder.hs: renders an animation to a .ca file
- Player.hs: can be used to play animaions from .ca files
- CellAutomaton.hs: handles a 3d cellular automaton
- GridRenderer.hs: turns a cellular automaton into a displayable grid of cells
- animations folder: contains already rendered .ca files

Helpers:
- Tree.hs: defines a tree structure, which is used in the implementation of the celllar automaton.
- Vector3.hs: handles vector operations in 3d. 

### Resources:
https://stackoverflow.com/questions/63662243/rgb-terminal-colors-with-haskell-and-brick -- Escape codes - colors/movement in the terminal
https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst -- Intro to IO and Monads
https://softologyblog.wordpress.com/2019/12/28/3d-cellular-automata-3/ -- Original idea
FP Tutorial 8 - ideas for the Tree structure
