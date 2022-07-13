STW-META is a thin wrapper around CONTEXTL. CONTEXTL is an extension of the Common Lisp Object System, (CLOS), which uses a meta object protocol, (MOP), to create a metaclass that allows for layered classes and their initialization in multiple defined contexts. STW-META extends CONTEXTL by providing a protocol for defining layered metaclasses. It does so, by defining the layered metaclass STW-BASE-CLASS, which uses the DEFINING-METACLASS BASE-CLASS, and is itself defined in the layered context STW-BASE-LAYER. All subsequent definitions of STW-BASE-CLASS rely on user defined layers.


* Protocol

1. Define layer context
2. Create defining metaclass
3. Append initargs for metaclass slots
4. Create slot definition class
5. Register slot definition class in layer context.
6. Define layered metaclass.
7. Defining a class
8. Initializing the class definition.


** 1. Define layer context

As context layers are classes they can inherit from other context layers and use user defined metaclasses. The use of user defined metaclasses enables dispatching on layers by type. Thus the layered functions ADJOIN-LAYER-USING-CLASS and REMOVE-LAYER-USING-CLASS can be called to enable mutual exclusion, complementary inclusion, or various combinations of each, between layers. Layers inheriting from STW-BASE-LAYER are by default mutually exclusive.

`(defclass foo-metaclass (stw-layer-context)
  ())`

`(deflayer foo-context (stw-base-layer)
  ()
  (:metaclass foo-metaclass))`

In the above definitions, FOO-METACLASS and FOO-CONTEXT are subtypes of STW-LAYER-CONTEXT and STW-BASE-LAYER respectively. Thus they inherit the rules established in the STW-META protocol.


** 2. Create defining metaclass

Using DEFCLASS or, if a specific layered context is desired, DEFINE-LAYERED-CLASS, create a class definition that inherits from BASE-CLASS 

`(define-layered-class bar
  :in foo-context (base-class)
  ((baz :initarg :baz :reader slot-definition-baz)))`

As I have defined bar here as a layered class all the functionality associated with layered classes are available. BAZ for example could have been declared special, or a layered slot, whilst slot-definition-baz could have been created as a layered reader. Source documentation on such features can be found in documentation listed below.


** 3. Append initargs for metaclass slots

In order for the initarg :baz to be valid in any subsequent layered metaclasses it must be registered.

`(defmethod partial-class-base-initargs append ((class db-wrap))
  '(:baz))`


** 4. Create slot definition class

Define a class that inherits from DB-BASE-COLUMN-DEFINITION.

`(defclass foo-context-slot-definition (db-base-column-definition)
  ((important :initarg :important :reader importantp)))`


** 5. Register slot definition class in layer context

The MOP method DIRECT-SLOT-DEFINITION-CLASS will look for the slot definition class to use. Should it not be defined all slots will be of type STANDARD-DIRECT-SLOT-DEFINITION.  

`(defmethod slot-definition-class ((layer-metaclass foo-metaclass))
  'foo-context-slot-definition)`


** 6. Define-layered metaclass

As STW-BASE-CLASS is a layered-class, the choice when defining the metaclass is to create a sibling class in a different layer, as in:

`(define-layered-class stw-base-class
  :in-layer foo-context (bar)
  ())`

or to define a metaclass that inherits stw-base-class but exists for the context of your application alone:

`(define-layered-class foobar
  :in-layer foo-context (stw-base-class bar)
  ())`


** 7. Defining a class

The macro DEFINE-BASE-CLASS is a wrapper on the macro DEFINE-LAYERED-CLASS, with a default metaclass of STW-BASE-CLASS. Slots can be passed as a combined list of symbols and cons. Initargs are automatically created, unless one is provided, using the keyword symbol of the slot name. The class definition is compiled within the layered environment of the definition, i.e. if the layer of a class is foo-context the class will be defined within the active layer foo-context. This aspect is relevant to the initialization process of the class definition.


** 8. Initializing the class definition - important!

As layered classes are subclasses of STANDARD-CLASS, initialization protocols proceed as per normal. As such, context specific initialization procedures should not be placed within initialize-instance, reinitialize-instance or shared-initialize methods, as they are called for each context in which a class is defined. To put it simply, they are not thread safe. Instead the layered function INITIALIZE-IN-CONTEXT is called from the auxiliary :around method of shared-initialize, and after the call to call-next-method. Context and class specific initialization procedures should be placed in specialized instances of this layered function. It is for this reason that layered classes of type STW-BASE-CLASS are defined within their layer context.



* Further Reading

For more on CONTEXTL layers, including reflective activation/deactivation, see:

(https://www.p-cos.net/documents/contextl-overview.pdf)
https://www.p-cos.net/documents/contextl-soa.pdf)
https://www.hirschfeld.org/writings/media/CostanzaHirschfeld_2007_ReflectiveLayerActivationInContextL_AuthorsVersionAcm.pdf)
https://www.p-cos.net/documents/special-full.pdf)

Additionally, see the test cases at (https://github.com/pcostanza/contextl/)

Finally, my gratitude goes to Pascal Costanza for both ContextL and Closer-Mop.
