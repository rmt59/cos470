# [[file:py_simulator.org::*Module setup][Module setup:1]]
from py_symbol import *
from py_messages import *
from random import randint
# Module setup:1 ends here

# [[file:py_simulator.org::*Module setup][Module setup:2]]
symbolGen = SymbolGenerator()
# Module setup:2 ends here

# [[file:py_simulator.org::*Object class: Simulated objects][Object class: Simulated objects:1]]
class Object():
# Object class: Simulated objects:1 ends here

# [[file:py_simulator.org::*Object class: Simulated objects][Object class: Simulated objects:2]]
    def __init__(self,name=None,location=[1,1], orientation="north",icon='@'):
        self.name = name if name else symbolGen.new_symbol("obj")
        self.location = location
        self.orientation = orientation
        self.icon_char = icon
        self.world = None
        self.mh = MessageHandler()
# Object class: Simulated objects:2 ends here

# [[file:py_simulator.org::*Object class: Simulated objects][Object class: Simulated objects:3]]
    def msg(self,m):
        self.mh.msg(m)
    def dmsg(self,m):
        self.mh.dmsg(m)
    def vmsg(self,m):
        self.mh.vmsg(m)
    def vdmsg(self,m):
        self.mh.vdmsg(m)
# Object class: Simulated objects:3 ends here

# [[file:py_simulator.org::*Object class: Simulated objects][Object class: Simulated objects:4]]
    def clock_tick(self):
        pass
# Object class: Simulated objects:4 ends here

# [[file:py_simulator.org::*Object class: Simulated objects][Object class: Simulated objects:5]]
    def icon(self):
        return self.icon_char
# Object class: Simulated objects:5 ends here

# [[file:py_simulator.org::*World class][World class:1]]
class WorldException(Exception):
    pass
class OutOfBounds(WorldException):
    pass
class LocationOccupied(WorldException):
    pass

class DirectionError(WorldException):
    pass

class OrientationError(WorldException):
    pass
# World class:1 ends here

# [[file:py_simulator.org::*World class][World class:2]]
class World():
    empty_char='.'
    side_wall_char='+'
    top_bottom_char='+'
    directions = ['north', 'east', 'south', 'west']

    def __init__(self,size=[10,10],num_obstacles=0,
                 obstacle_locations=None):
        self.size = size
        self.num_obstacles = num_obstacles
        self.obstacle_locations = obstacle_locations

        self.objects = []

        self.mh = MessageHandler()
# World class:2 ends here

# [[file:py_simulator.org::*World class][World class:3]]
    def msg(self,m):
        self.mh.msg(m)
    def dmsg(self,m):
        self.mh.dmsg(m)
    def vmsg(self,m):
        self.mh.vmsg(m)
    def vdmsg(self,m):
        self.mh.vdmsg(m)
# World class:3 ends here

# [[file:py_simulator.org::*World class][World class:4]]
    def set_drawing_character(self,empty=None,side_wall=None,
                              top_bottom=None):
        self.empty_char = empty if empty else World.empty_char
        self.side_wall_char = side_wall if side_wall \
            else World.side_wall_char
        self.top_bottom_char = top_bottom if top_bottom else \
            World.top_bottom_char
# World class:4 ends here

# [[file:py_simulator.org::*World class][World class:5]]
    def empty(self,location):
        if not self.in_bounds(location):
            return False
        else:
            for object in self.objects:
                if object.location == location:
                    return False
            return True
# World class:5 ends here

# [[file:py_simulator.org::*World class][World class:6]]
    def in_bounds(self,loc):
        (x,y) = loc
        (max_x,max_y) = self.size
        return False if x < 1 or y < 1 or x > max_x or y > max_y else True
# World class:6 ends here

# [[file:py_simulator.org::*World class][World class:7]]
    def add_object(self,object):
        if type(object) == list or type(object) == tuple:
            object = Object(location=object)

        self.vdmsg(f'(adding object {object.name} to world)')

        object.world = self                  # so it can do its own percepts

        if not self.in_bounds(object.location):
            raise OutOfBounds()
        elif not self.empty(object.location):
            raise LocationOccupied
        else:
            self.objects.append(object)
# World class:7 ends here

# [[file:py_simulator.org::*World class][World class:8]]
    def clear(self):
        self.vdmsg('(clearing world)')
        self.objects = []
# World class:8 ends here

# [[file:py_simulator.org::*World class][World class:9]]
    def object_locations(self):
        return [obj.location for obj in self.objects]
# World class:9 ends here

# [[file:py_simulator.org::*World class][World class:10]]
    def delete_object(self,object):
        return self.remove_object(object)

    def remove_object(self,object):
        object = self.find_object(object)
        if not object:
            self.vdmsg(f'(remove_object: object {object.name} not found)')
            return None
        else:
            i = self.objects.index(object)
            self.objects = self.objects[0:i] + self.objects[i+1:]
            self.vdmsg(f'(remove_object: removed {object.name})')
            return object
# World class:10 ends here

# [[file:py_simulator.org::*World class][World class:11]]
    def find_object(self,description):
        if type(description) == list:
            return self.find_object_by_location(description)
        else:
            for obj in self.objects:
                if obj is description:
                    return obj
            return None

    def find_object_by_location(self,loc):
        for obj in self.objects:
            if loc == obj.location:
                return obj
        return None
# World class:11 ends here

# [[file:py_simulator.org::*World class][World class:12]]
    def draw(self):
        self.draw_line(self.top_bottom_char)
        self.draw_rows(self.empty_char,self.side_wall_char)
        self.draw_line(self.top_bottom_char)
        
    def draw_line(self,char):
        print((self.size[1]+2)*char)

    def draw_rows(self,empty,wall):
        for i in range(self.size[0]):
            print(wall,end='')
            self.draw_row(i+1,empty)
            print(wall)

    def draw_row(self,row,empty):
        for col in range(self.size[1]):
            obj = self.find_object([row,col+1])
            if obj:
                print(obj.icon(),end='')
            else:
                print(empty,end='')
# World class:12 ends here

# [[file:py_simulator.org::*World class][World class:13]]
    # return empty location
    def empty_location(self):
        for i in range(self.size[0]*self.size[1]):
            loc = [randint(1,self.size[0]),randint(1,self.size[0])]
            if self.empty(loc):
                return loc
        self.dmsg('No empty squares found after row*column tries.')
        return None
# World class:13 ends here

# [[file:py_simulator.org::*World class][World class:14]]
    # Note: we're going w/ row,column rather than x,y now:
    def next_location(self,location,direction):
        if direction == 'north':
            return [location[0]-1,location[1]]
        elif direction == 'south':
            return [location[0]+1,location[1]]
        elif direction == 'east':
            return [location[0],location[1]+1]            
        elif direction == 'west':
            return [location[0],location[1]-1]
        else:
            raise DirectionError()

    def opposite_direction(self,direction):
        if direction == 'north':
            return 'south'
        elif direction == 'south':
            return 'north'
        elif direction == 'east':
            return 'west'
        elif direction == 'west':
            return 'east'
        else:
            raise OrientationError()

    def clockwise_direction(self,direction):
        if direction == 'north':
            return 'east'
        elif direction == 'south':
            return 'west'
        elif direction == 'east':
            return 'south'
        elif direction == 'west':
            return 'north'
        else:
            raise DirectionError()

    def counterclockwise_direction(self,direction):
        return self.opposite_direction(self.clockwise_direction(direction))
# World class:14 ends here

# [[file:py_simulator.org::*World class][World class:15]]
    def set_drawing_character(self,empty=None,side_wall=None,
                              top_bottom=None):
        self.world(set_drawing_character(empty=empty,side_wall=side_wall,
                                         top_bottom=top_bottom))
# World class:15 ends here

# [[file:py_simulator.org::*Simulator class][Simulator class:1]]
class Simulator():
    def __init__(self,size=[10,10],num_obstacles=0,obstacle_locations=None):
        self.time = 0
        self.world = World(size=size,num_obstacles=num_obstacles,
                           obstacle_locations=obstacle_locations)
        self.mh = MessageHandler()
# Simulator class:1 ends here

# [[file:py_simulator.org::*Simulator class][Simulator class:2]]
    def msg(self,m):
        self.mh.msg(m)
    def dmsg(self,m):
        self.mh.dmsg(m)
    def vmsg(self,m):
        self.mh.vmsg(m)
    def vdmsg(self,m):
        self.mh.vdmsg(m)
# Simulator class:2 ends here

# [[file:py_simulator.org::*Simulator class][Simulator class:3]]
    def clear(self):
        self.world.clear()
        self.msg('Cleared.')

    def reset(self):
        self.clear()
        self.time = 0
# Simulator class:3 ends here

# [[file:py_simulator.org::*Simulator class][Simulator class:4]]
    def add_obstacles(self,loc_list):
        return self.add_objects(loc_list)

    # "loc_list" can be a list of locations or actual object instances:
    def add_objects(self,loc_list):
        for loc in loc_list:
            self.world.add_object(loc)

    def add_object(self,loc_or_obj):
        return self.world.add_object(loc_or_obj)

    def add_random_obstacles(self,number=None,max=20,min=1):
        if number == None:
            number = randint(min,max)
        for i in range(number):
            self.add_random_obstacle()

    def add_random_obstacle(self):
        self.world.add_object(self.world.empty_location())

    def add_robot(self,robot=None,name=None,location=None,orientation=None,
                  robot_type='Robot'):
        if location and not self.empty(location):
            self.msg(f"Can't add robot at {location}: not empty or out of bounds.")
            return False
        if robot is None:
            robot = eval(f'{robot_type}()')
            robot.location = location if location else self.world.empty_location()
            robot.orientation = orientation if orientation else self.world.directions[randint(0,3)]
        else:
            if location:
                robot.location = location
            if orientation:
                robot.orientation = orientation

        self.dmsg(f'Adding robot {robot.name} at {robot.location}, orientation {robot.orientation}')
        return self.add_object(robot)
# Simulator class:4 ends here

# [[file:py_simulator.org::*Simulator class][Simulator class:5]]
    def empty(self,location):
        return self.world.empty(location)

    def empty_location(self):
        return self.world.empty_location()

    def find_object(self,description):
        return self.world.find_object(description)

    def delete_object(self,object):
        self.world.delete_object(object)

    def remove_object(self,object):
        self.world.delete_object(object)

    def random_location(self):
        return [randint(1,self.world.size[0]),randint(1,self.world.size[1])]

    def random_empty_location(self):
        self.world.empty_location()


    def draw(self,empty_char='.',side_wall_char='+',top_bottom_char='+'):
        self.world.draw()
# Simulator class:5 ends here

# [[file:py_simulator.org::*Simulator class][Simulator class:6]]
    def run(self,ticks=1,show_each=False):
        self.msg(f'Running for {ticks} ticks.')
        for i in range(ticks):
            self.clock_tick()
            if show_each:
                self.draw()
# Simulator class:6 ends here

# [[file:py_simulator.org::*Simulator class][Simulator class:7]]
    def clock_tick(self):
        self.dmsg('.')
        for object in self.world.objects:
            object.clock_tick()
        self.time += 1
# Simulator class:7 ends here

# [[file:py_simulator.org::*Robot class][Robot class:1]]
class Robot(Object):
# Robot class:1 ends here

# [[file:py_simulator.org::*Robot class][Robot class:2]]
    command_map = {"nop": "do_nop",
                   "forward": "do_move_forward",
                   "backward": "do_move_backward", 
                   "left": "do_move_left", 
                   "right": "do_move_right", 
                   "turn_right": "do_turn_clockwise",
                   "turn_left": "do_turn_counterclockwise"}

    percept_map = {"front_sensor": "forward_sensor", 
                   "front_bump": "front_bump_sensor", 
                   "rear_bump": "rear_bump_sensor", 
                   "right_bump": "right_bump_sensor", 
                   "left_bump": "left_bump_sensor"}
# Robot class:2 ends here

# [[file:py_simulator.org::*Robot class][Robot class:3]]
    def __init__(self,command_map=None,percept_map=None,
                 location=[1,1],orientation='north',
                 name=None):
        super().__init__(location=location, orientation=orientation)
        self.percept = None
        self.next_action = None
        self.prev_action = None
        self.prev_action_success = None
        
        self.command_map = command_map if command_map else \
            Robot.command_map
        self.percept_map = percept_map if percept_map else \
            Robot.percept_map

        self.name = name if name else symbolGen.new_symbol('robot')
# Robot class:3 ends here

# [[file:py_simulator.org::*Robot class][Robot class:4]]
    def calculate_percept(self):
        percept = []
        for sensor in self.percept_map:
            func = self.percept_map[sensor]
            self.vdmsg(f'(calculate_percept({self.name}): calculating {sensor} value)')
            percept.append([sensor, eval(f'self.{func}()')])
        self.percept = percept
        return percept
# Robot class:4 ends here

# [[file:py_simulator.org::*Robot class][Robot class:5]]
    def icon(self):
        if self.orientation == 'north':
            return '^'
        elif self.orientation == 'south':
            return 'v'
        elif self.orientation == 'east':
            return '>'
        elif self.orientation == 'west':
            return '<'
        else:
            return '?'
# Robot class:5 ends here

# [[file:py_simulator.org::*Robot class][Robot class:6]]
    def clock_tick(self):
        self.calculate_percept()
        self.next_action = self.agent_program(self.percept)
        self.take_action()
        return True
# Robot class:6 ends here

# [[file:py_simulator.org::*Robot class][Robot class:7]]
    def agent_program(self,percept):
        self.msg(f'{self.name}: Dummy agent_program({percept}) called.')
        return 'nop'
# Robot class:7 ends here

# [[file:py_simulator.org::*Robot class][Robot class:8]]
    def forward_sensor(self):
        if self.world.empty(self.world.next_location(self.location,
                                                     self.orientation)):
            return False
        else:
            return True

    def front_bump_sensor(self):
        return self.bump_sensor('forward',self.orientation)
    def rear_bump_sensor(self):
        return self.bump_sensor('backward',self.world.opposite_direction(self.orientation))
    def left_bump_sensor(self):
        return self.bump_sensor('left', self.world.counterclockwise_direction(self.orientation))
    def right_bump_sensor(self):
        return self.bump_sensor('right', self.world.clockwise_direction(self.orientation))

    def bump_sensor(self,which,direction):
        return self.prev_action == which and \
            not self.prev_action_success and \
            not self.world.empty(self.world.next_location(self.location, direction))

    ## Action methods:
    def take_action(self):
        if not self.next_action in self.command_map:
            self.msg(f'take_action for {self.name}: unknown action {self.next_action}; ' + \
                'doing nothing')
            self.next_action = "nop"
            self.prev_action_success = False
        else:
            method = self.command_map[self.next_action]
            self.msg(f'{self.name}: Performing action {self.next_action}')
            self.dmsg(f'(take_action: calling method {method})')
            self.prev_action_success = eval(f'self.{method}()')

        self.prev_action = self.next_action
        self.next_action = None
        return self.prev_action_success
# Robot class:8 ends here

# [[file:py_simulator.org::*Robot class][Robot class:9]]
    ## actions implementation:
    def do_nop(self):
        return True

    def do_move_forward(self):
        world = self.world
        return self.move(world.next_location(self.location,self.orientation))

    def do_move_backward(self):
        world = self.world
        return \
            self.move(world.next_location(self.location,
                                          world.opposite_direction(self.orientation)))

    def do_move_left(self):
        world = self.world
        return \
            self.move(world.next_location(self.location,
                                          world.counterclockwise_direction(self.orientation)))

    def do_move_right(self):
        world = self.world
        return \
            self.move(world.next_location(self.location,
                                          world.clockwise_direction(self.orientation)))

    def move(self,location):
        if not self.world.empty(location):
            self.msg(f'{self.name}: Tried and failed to move to {location}.')
            return False
        else:
            self.location = location
            self.msg(f'{self.name} Moving to {location}.')
            return True

    def do_turn_clockwise(self):
        self.orientation = self.world.clockwise_direction(self.orientation)
        self.msg(f'{self.name}: Turning right to {self.orientation}.')
        return True

    def do_turn_counterclockwise(self):
        self.orientation = self.world.counterclockwise_direction(self.orientation)
        self.msg(f'{self.name}: Turning left to {self.orientation}.')
        return True
# Robot class:9 ends here

# [[file:py_simulator.org::*=create_simulator= function][=create_simulator= function:1]]
def create_simulator(size=[10,10],num_obstacles=0,obstacle_locations=None):
    return Simulator(size=size,num_obstacles=num_obstacles,obstacle_locations=obstacle_locations)
# =create_simulator= function:1 ends here

# [[file:py_simulator.org::*Example: =RandomRobot=][Example: =RandomRobot=:1]]
class RandomRobot(Robot):
    def __init__(self,command_map=None,percept_map=None,
                 location=[1,1],orientation='north',
                 name=None):
# Example: =RandomRobot=:1 ends here

# [[file:py_simulator.org::*Example: =RandomRobot=][Example: =RandomRobot=:2]]
        super().__init__(command_map=command_map, percept_map=percept_map,
                                   location=location, orientation=orientation,
                                   name=symbolGen.new_symbol('randrob'))
# Example: =RandomRobot=:2 ends here

# [[file:py_simulator.org::*Example: =RandomRobot=][Example: =RandomRobot=:3]]
    def agent_program(self,percept):
        # Just wander around:
        keys = list(self.command_map.keys())
        self.next_action = keys[randint(0,len(keys)-1)]

        # here is how you can use msg, dmsg, etc.:
        self.dmsg(f'{self.name}: next action={self.next_action}.')

        return self.next_action              # must do this!!
# Example: =RandomRobot=:3 ends here
