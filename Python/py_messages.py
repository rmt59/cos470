# [[file:python_messages.org::*Description][Description:1]]
class MessageHandler():
    verbosity_map = {"silent": 0,
                     "minimum": 0,
                     "normal": 10,
                     "verbose": 20,
                     "debugging": 30,
                     "verbose_debugging": 40,
                     "maximum": float('inf')}

    # Class methods to translate verbosities <-> numbers
    def verbNum(verbosity):
        if verbosity in MessageHandler.verbosity_map:
            return MessageHandler.verbosity_map[verbosity]
        else:
            print(f'Invalid verbosity "{verbosity}"; using "normal".')
            return MessageHandler.verbosity_map['normal']

    def numVerb(num):
        for key in MessageHandler.verbosity_map:
            if num == MessageHandler.verbosity_map[key]:
                return key
        print(f"Can't find name for verbosity number {num}; using 'normal'.")
        return 'normal'



    def __init__(self,verbosity='normal'):
        self.current_verbosity = MessageHandler.verbNum(verbosity)

    def set_verbosity(self,verbosity):
        self.current_verbosity = MessageHandler.verbNum(verbosity)

    def verbosity(self,numeric=False):
        if numeric:
            return self.current_verbosity
        else:
            return MessageHandler.numVerb(self.current_verbosity)

    def quash_msg(self,msg_verb):
        return False if self.current_verbosity >= MessageHandler.verbNum(msg_verb) else True

    def msg(self,*args):
        self.msg_common('normal',args)

    def vmsg(self,*args):
        self.msg_common('verbose',args)

    def dmsg(self,*args):
        self.msg_common('debugging',args)

    def vdmsg(self,*args):
        self.msg_common('verbose_debugging',args)

    def msg_common(self,verb,args):
        if self.quash_msg(verb):
            return None
        else:
            for thing in args:
                print(thing,end=' ')
            print()
            return None
# Description:1 ends here
