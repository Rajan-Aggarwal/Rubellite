if a == 5
  b = 3
else
  b = 5

def counter
    string _name
    int _count
    def init()
        self._count = 50
        self._name = "container of elements"
    def add(int element)
        self._count = self._count + element
    def sub(int element)
        self._count = self._count - element
    def get() : int
        return self._count
    def name() : string
        return self._name
    def _init(int count, string name)
        self._count = count
self._name = name

counter c
c._count = 7
c._name = "test"