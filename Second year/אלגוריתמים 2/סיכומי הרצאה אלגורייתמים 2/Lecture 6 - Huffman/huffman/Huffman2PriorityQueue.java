package huffman;
import java.util.PriorityQueue;
// the array of frequencies is sorted (increasing sort)
// smallest element is in front of queue
public class Huffman2PriorityQueue {

	private class Node implements Comparable<Node>{
		Integer _element; // frequency
		int _index;
		public Node(int element, int index){
			_element = element;
			_index = index;//0-left child, 1-right child 
		}
		public String toString(){
			return "(w="+_element+",i="+_index+")";
		}
		@Override
		public int compareTo(Node f) {
			// TODO Auto-generated method stub
			return _element - f._element;
		}
	}// end class Node

	int _mat[][];
	char _letters[];
	int _n, _nMax;
	PriorityQueue<Node> q1;
	PriorityQueue<Node> q2;
	String _code[];
	// constructor
	public Huffman2PriorityQueue(Integer[] freq, char [] letters){
		_n = freq.length;
		_letters = new char[_n];
		_code = new String[_n];
		_nMax = 2*_n - 1;
		_mat = new int[2*_n-1][4];	//table: _mat[][0]- parent index
		q1 = new PriorityQueue<Node>(_nMax);
		q2 = new PriorityQueue<Node>(_nMax);
		for (int i = 0; i < freq.length; i++) {//O(n)
			_mat[i][0] = freq[i];
			_letters[i] = letters[i];
			_code[i] = new String();
			q1.add(new Node(freq[i], i));
		}
		System.out.println(q1.toString());
	}
	public void buildTable(){
		Node x1 = q1.remove();
		Node x2 = q1.remove();
		int parent = _n;
		int weight = x1._element + x2._element;
		q2.add(new Node(weight, parent));
		_mat[parent][0] = weight;
		_mat[parent][1] = x1._index;  //left child (0)
		_mat[parent][2] = x2._index;  //right child (1)
		_mat[x1._index][3] = parent;  // parent
		_mat[x2._index][3] = parent;  // parent
		parent++;
		while (q1.size() + q2.size()>1){
			x1 = nextMin();
			x2 = nextMin();
			weight = x1._element + x2._element;
			q2.add(new Node(weight, parent));
			_mat[parent][0] = weight;
			_mat[parent][1] = x1._index;  //left child (0)
			_mat[parent][2] = x2._index;  //right child (1)
			_mat[x1._index][3] = parent;  // parent
			_mat[x2._index][3] = parent;  // parent
			parent++;
		}
	}
	private Node nextMin(){
		Node x, y;
		if (q1.isEmpty())	x = q2.remove();  
		else if (q2.isEmpty()) x = q1.remove();  
		else{
			x = q1.peek();
			y = q2.peek();
			if (x._element > y._element) x = q2.remove();
			else x = q1.remove();
		}
		return x;
	}
	// build the Huffman's Code for all letters
	public void huffmanCode(){
		for (int i=0; i<_n; i++){
			int child = i;
			int parent = _mat[child][3];
			while(parent!=0){
				if (_mat[parent][1]==child) _code[i]="0" + _code[i];
				else _code[i]="1" + _code[i];	
				child = parent;
				parent = _mat[child][3];
			}
		}
	}
	// build the Huffman's Code for all letters
	public void huffmanCodeRecurs(){
		huffmanCodeRecurs(2*_n - 2, "");
	}
	public void huffmanCodeRecurs(int x, String s){
		if (x >= _n){// x in not a leaf
			huffmanCodeRecurs(_mat[x][1], s + "0");//left child
			huffmanCodeRecurs(_mat[x][2], s + "1");//right child
		}
		else{
			_code[x] = s;
		}

	}
	// print the table
	public void printMat(){
		for (int i=0; i<_n*2-1; i++){
			for (int j=0; j<4; j++){
				System.out.print(_mat[i][j]+" ");
			}
			System.out.println();
		}
	}
	// print the Huffman's Codes
	public void printCode(){
		for (int i=0; i<_n; i++){
			System.out.println(_letters[i]+": "+_code[i]);
		}
	}
	public static void main(String[] args) {
		//Integer freq2[] = {5,9,12,13,16,45};
		//char letter2[] = {'f','e','c','b','d','a'};
		Integer freq2[] = {16,13,12,9,5,45};
		char letter2[] = {'d','b','c','e','f','a'};
		Huffman2PriorityQueue hq = new Huffman2PriorityQueue(freq2, letter2);
		hq.buildTable();
		hq.printMat();
		//hq.huffmanCode();
		hq.huffmanCodeRecurs();
		hq.printCode();
		//////
		Integer freq1[] = {8,12,15,25,40};
		char letter1[] = {'d','a','c','e','b'};
		Huffman2PriorityQueue hq1 = new Huffman2PriorityQueue(freq1, letter1);
		hq1.buildTable();
		hq1.printMat();
		//hq.huffmanCode();
		hq1.huffmanCodeRecurs();
		hq1.printCode();

	}
}
/**
	[(w=5,i=0), (w=9,i=1), (w=12,i=2), (w=13,i=3), (w=16,i=4), (w=45,i=5), ]
	5 0 0 6 
	9 0 0 6 
	12 0 0 7 
	13 0 0 7 
	16 0 0 8 
	45 0 0 10 
	14 0 1 8 
	25 2 3 9 
	30 6 4 9 
	55 7 8 10 
	100 5 9 0 
		f: 1100
		e: 1101
		c: 100
		b: 101
		d: 111
		a: 0
	------------------------------------------
	[(w=8,i=0), (w=12,i=1), (w=15,i=2), (w=25,i=3), (w=40,i=4), ]
	8 0 0 5 
	12 0 0 5 
	15 0 0 6 
	25 0 0 7 
	40 0 0 8 
	20 0 1 6 
	35 2 5 7 
	60 3 6 8 
	100 4 7 0 
		d: 1110
		a: 1111
		c: 110
		e: 10
		b: 0

 */