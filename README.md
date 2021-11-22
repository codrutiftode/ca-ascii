# 3D Cellular Automaton in ASCII Art
### Main idea
The program records a 2-state 3D cellular automaton using a Von Neumann neighbourhood as it evolves through a number of stages.
The automaton is represented as the list of 3d vectors corresponding to the alive cells in the automaton.<br>
Then, the automaton is converted to a grid of characters that can be displayed in the terminal. 
This is done by rotating the CA around the Z (vertical) axis by an ever increasing angle, then around the X-axis by the angle that the point of view makes with the XY plane.<br> 
This causes an effect similar to a top-down view, where the x and y coordinates of the vectors can be correlated with x and y positions on the screen, and the z coordinate gives the depth which can be used in the coloring of pixels.<br><br> 
The projection mode is orthographic, but a sense of perspective is induced by using a dimmer color for pixels that are further away from the point of view.

### Explanation of different files
- Recorder.hs: renders an animation to a .ca file
- Player.hs: can be used to play animaions from .ca files
- CellAutomaton.hs: handles a 3d cellular automaton
- GridRenderer.hs: turns a cellular automaton into a displayable grid of cells
- animations folder: contains already rendered .ca files
- output_videos folder: contains some screen-recorded .ca animations that have been run in the terminal

Helpers:
- Tree.hs: defines a tree structure, which is used in the implementation of the celllar automaton.
- Vector3.hs: handles vector operations in 3d. 

### Execution pre-requisites
The Square font (https://strlen.com/square/). This needs to be installed and the terminal has to be configured to use it in order to get proper output. Usual fonts have characters of bigger height than width, but the program needs the width and height of characters to be equal to display properly. Otherwise, the output will still be valid, just elongated in an unsatisfying way.

### Resources:
https://stackoverflow.com/questions/63662243/rgb-terminal-colors-with-haskell-and-brick -- Escape codes - colors/movement in the terminal<br>
https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst -- Intro to IO and Monads<br>
https://www.haskell.org/tutorial/io.html -- IO and Monads <br>
https://softologyblog.wordpress.com/2019/12/28/3d-cellular-automata-3/ -- Original idea<br>
Square font: https://strlen.com/square/
FP Tutorial 8 - ideas for the Tree structure
