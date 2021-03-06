/* Name: Avery Wong

   UID: 904582269

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*/

// import lists and other data structures from the Java standard library
import java.util.*;

// PROBLEM 1

// a type for arithmetic expressions
interface Exp {
    double eval(); 	                       // Problem 1a
    List<Instr> compile(); 	               // Problem 1c
}

class Num implements Exp {
    protected double val;

    public boolean equals(Object o) { return (o instanceof Num) && ((Num)o).val == this.val; }
  
    public String toString() { return "" + val; }

    public Num( double x){ val = x;}

    public double eval(){return val;}

    public List<Instr> compile(){
	List<Instr> m_list = new ArrayList<Instr>();
        m_list.add(new Push(val));
        return m_list;
    }
    
}

class BinOp implements Exp {
    protected Exp left, right;
    protected Op op;

    public boolean equals(Object o) {
    	if(!(o instanceof BinOp))
    		return false;
    	BinOp b = (BinOp) o;
    	return this.left.equals(b.left) && this.op.equals(b.op) &&
		    	this.right.equals(b.right);
    }

    public String toString() {
		return "BinOp(" + left + ", " + op + ", " + right + ")";
    }

    public BinOp(Exp m_left, Op m_op, Exp m_right) { left = m_left; op = m_op; right = m_right;}

    public double eval(){return op.calculate(left.eval(),right.eval());}

    public List<Instr> compile(){
	List<Instr> m_list = new ArrayList<Instr>();
        m_list.addAll(left.compile());
        m_list.addAll(right.compile());
	m_list.add(new Calculate(op));
        return m_list;
    }
}

// a representation of four arithmetic operators
enum Op {
    ADD, SUB, MULT, DIV;

	double calculate(double x, double y) {
        switch(this) {
            case ADD:   return x + y;
            case SUB:  return x - y;
            case MULT:  return x * y;
            case DIV: return x / y;
        }
        throw new AssertionError("Unknown Op: " + this);
    }
}

// a type for arithmetic instructions
interface Instr {

    public void eval_instr(Stack<Double> s);
}

class Push implements Instr {
    protected double val;

	public boolean equals(Object o) { return (o instanceof Push) && ((Push)o).val == this.val; }

    public String toString() {
		return "Push " + val;
    }

    public Push(double x ){val = x;}

    public void eval_instr(Stack<Double> s){s.push(new Double(val));}

}

class Calculate implements Instr {
    protected Op op;

    public boolean equals(Object o) { return (o instanceof Calculate) && 
    						  ((Calculate)o).op.equals(this.op); }

    public String toString() {
		return "Calculate " + op;
    }

    public Calculate(Op o_op) {op = o_op;}

    public void eval_instr(Stack<Double> s){
	double x = s.pop().doubleValue();
	double y = s.pop().doubleValue();
	s.push(new Double((op.calculate(x,y))));
    }
   
}

class Instrs {
    protected List<Instr> instrs;

    public Instrs(List<Instr> instrs) { this.instrs = instrs; }

     public double execute() {
	 Stack<Double> m_stack = new Stack<Double>();

	 for (Instr instr_obj: instrs){
	     instr_obj.eval_instr(m_stack);
	 }
	 double result = 0;
	 result = m_stack.pop().doubleValue();
	 return result;
     }  // Problem 1b
}


class CalcTest {
    public static void main(String[] args) {
	 //    // a test for Problem 1a
		Exp exp =
	     	new BinOp(new BinOp(new Num(1.0), Op.ADD, new Num(2.0)),
		    	  	  Op.MULT,
		       	  new Num(3.0));
		//		System.out.println(exp.eval());
		 assert(exp.eval() == 9.0);

		// // a test for Problem 1b
		 List<Instr> is = new LinkedList<Instr>();
		 is.add(new Push(1.0));
		 is.add(new Push(2.0));
		 is.add(new Calculate(Op.ADD));
		 is.add(new Push(3.0));
		 is.add(new Calculate(Op.MULT));
		 Instrs instrs = new Instrs(is);
		 // System.out.println(instrs.execute());
		 assert(instrs.execute() == 9.0);

		// // a test for Problem 1c
		 assert(exp.compile().equals(is));

		 
    }
}


// PROBLEM 2

// the type for a set of strings
interface Set {
     int size();
     boolean contains(String s);
     void add(String s);
    // void printlist();
}

// an implementation of Set using a linked list
class ListSet implements Set {
   
    protected SNode head;
    protected int m_size;

    ListSet() {
        this.head = new SEmpty();
	this.m_size = 0;
    }
    
    public int size(){
        return m_size;
    }

    public boolean contains(String s){
	return head.exists(s);
    }

    public void add(String s){
	this.head = head.append(s);
	m_size = head.count();
    }
    /*
    public void printlist(){
	head.printnode();
    }
    */
     
}

// a type for the nodes of the linked list
interface SNode {
    int count();
    boolean exists(String word);
    SNode append(String s);
    //void printnode();
}

// represents an empty node (which ends a linked list)
class SEmpty implements SNode {
    SEmpty() {}

    public int count(){
	return 0;
    }
    public boolean exists(String word){
	return false;
    }

    public SNode append(String s){
	SNode n_node = new SElement(s,new SEmpty());
	return n_node;
    }
    /*
    public void printnode()
    {
	System.out.println("END");
    }
    */
}

// represents a non-empty node
class SElement implements SNode {
    protected String elem;
    protected SNode next;

    SElement(String elem, SNode next) {
        this.elem = elem;
        this.next = next;
    }

    public int count(){
	return this.next.count()+1;
    }

    public boolean exists(String word){
	if (this.elem.compareTo(word) == 0)
	    return true;
	else if (this.elem.compareTo(word) > 0){
	    return false;
	}
	else {
	    return this.next.exists(word);
	}
    }

    public SNode append(String s){
	
	
     	if ((this.elem.compareTo(s)) == 0){
	    return this;
        }
	else if ((this.elem.compareTo(s)) < 0){
	    this.next = this.next.append(s);
	    return this;
	}
	else{
	    SNode n_node = new SElement(s,this);
	    return n_node; 
	}

    }
    /*
    public void printnode()
    {
	System.out.println(this.elem);
	this.next.printnode();
    }
    */
}
/*
class test2 {

        public static void main(String[] args) {
	    Set set1 = new ListSet();
	    System.out.println("printing size\n");
	    System.out.println(set1.size());
	    System.out.println("adding: abs then aa\n");
	    set1.add("abs");
	    set1.add("aa");
	    System.out.println("printing size\n");
	    System.out.println(set1.size());
	    System.out.println("adding abs again\n");
	    set1.add("abs");
	    System.out.println("printing size again\n");
	    System.out.println(set1.size());
	    System.out.println("adding: ball\n");
	    set1.add("ball");
	    System.out.println("printing size");
	    System.out.println(set1.size());
	    System.out.println("adding ball again");
	    set1.add("ball");
	    System.out.println("printing size");
	    System.out.println(set1.size());
	    System.out.println("adding cat then dog and then dead");
	    set1.add("cat");
	    set1.add("dog");
	    set1.add("dead");
	    System.out.println("printing size");
	    System.out.println(set1.size());
	    set1.printlist();
	}

}

*/
