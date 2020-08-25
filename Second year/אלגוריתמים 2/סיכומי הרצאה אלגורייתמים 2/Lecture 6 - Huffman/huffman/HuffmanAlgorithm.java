package huffman;

public class HuffmanAlgorithm {
	int _n;
	int [][] _mat;
	int [] _freq;
	String _code[];
	// constructor
	public HuffmanAlgorithm(int freq[]){
		_n = freq.length;
		_freq = new int[2*_n-1];// array of frequencies
		_mat = new int[2*_n-1][3];	//table: _mat[][0]- parent index
		  							//     _mat[][1]- left child index (0)  
		  							//     _mat[][2]- right child index (1)  
		_code = new String[_n];		// Huffman Code
		for (int i=0; i<_n; i++){
			_freq[i] = freq[i];
			_code[i] = "";
		}
	}
	// create the table 
	public void HuffmanTable(){
		int [] min12 = new int[2];
		int ind1, ind2;
		int parent = _n;
		while (parent < _freq.length){
			min12 = twoMinIndexes(parent);
			ind1 = min12[0];  
			ind2 = min12[1];
			_mat[parent][1] = ind1;  //left child (0)
			_mat[parent][2] = ind2;  //right child (1)
			_mat[ind1][0] = parent;  // parent
			_mat[ind2][0] = parent;  // parent
			_freq[parent] = _freq[ind1] +_freq[ind2];
			_freq[ind1] = - _freq[ind1];// not in use
			_freq[ind2] = - _freq[ind2];// not in use
			parent++;
		}
	}
	// build the Huffman's Code for all letters
	public void HuffmanCode(){
		for (int i=0; i<_n; i++){
			int child = i;
			int parent = _mat[child][0];
			while(parent!=0){
				if (_mat[parent][1]==child) _code[i]=_code[i]+"0";
				else _code[i]=_code[i]+"1";	
				child = parent;
				parent = _mat[child][0];
			}
		}
	}
	// find the first unused vertex (by frequency) 
	private int next1(int start){
		while(start<_freq.length && _freq[start] < 0) start++;
		return start;
	}
	// find the second unused vertex (by frequency) 
	private int next2(int start, int ind1){
		while(start<_freq.length && (_freq[start]<0 || _freq[start]==_freq[ind1])){
			start++;
		}
		return start;
	}
	// find indexes of two smallest unused frequencies
	// assumption: array length >= 2
	private int[] twoMinIndexes(int to){
		int ind1, ind2, start;
		// initialization
		ind1 = next1(0);
		ind2 = next2(ind1+1, ind1);
		start = ind2;
		if (_freq[ind1] > _freq[ind2]){
			int t = ind1;
			ind1 = ind2;
			ind2 = t;
		}
		// the main loop
		for (int i=next1(start+1); i<to; i=next1(i+1)){
			if (_freq[i] < _freq[ind1]){
				ind2 = ind1;
				ind1 = i;
			}
			else if (_freq[i]<_freq[ind2]){
				ind2 = i;
			}
		}
		// the result:
		int []min12 = new int[2];
		min12[0] = ind1;
		min12[1] = ind2;
		return min12;
	}
	// print the table
	public void printMat(){
		for (int i=0; i<_n*2-1; i++){
			for (int j=0; j<3; j++){
				System.out.print(_mat[i][j]+" ");
			}
			System.out.println();
		}
	}
	// print the Huffman's Codes
	public void printCode(){
		for (int i=0; i<_n; i++){
			System.out.println((char)('a'+i)+": "+_code[i]);
		}
	}
/////////////////	
	// Search for two smallest elements: 
	public static int[] twoMinElements(int arr[]){
		int min1, min2;
		// assumption: array length >= 2
		// initialization
		min1 = arr[0];
		min2 = arr[1];
		if (arr[0]>arr[1]){
			min1 = arr[1];
			min2 = arr[0];
		}
		// the main loop
		for (int i=2; i<arr.length; i++){
			if(arr[i] < min1){
				min2=min1;
				min1=arr[i];
			}
			else if(arr[i]<min2){
				min2=arr[i];
			}
		}
		// the result:
		int []min12 = new int[2];
		min12[0] = min1;
		min12[1] = min2;
		return min12;
	}
	
	///////// testing
	public static void checkTwoMin(){
		// two smallest elements
		int size = 10;
		int arr[] = MyLibrary.randomIntegerArray(size);
		MyLibrary.printIntegerArray(arr);
		int[] min12 = twoMinElements(arr);
		MyLibrary.printIntegerArray(min12);
		
	}
	public static void check2Min(){
		// two smallest elements
		int size = 10;
		int arr[] = MyLibrary.randomIntegerArray(size);
		MyLibrary.printIntegerArray(arr);
		HuffmanAlgorithm ha =new HuffmanAlgorithm(arr);
		int[] min12 = ha.twoMinIndexes(size);
		MyLibrary.printIntegerArray(min12);
		
	}
	public static void main(String[] args) {
		//checkTwoMin();
		//check2Min();
		int freq1[] = {12,40,15,8,25};
		int freq2[] = {45,13,12,16,9,5};
		int freq3[] = {15,7,6,6,5};
		HuffmanAlgorithm ha = new HuffmanAlgorithm(freq1);
		ha.HuffmanTable();
		ha.printMat();
		ha.HuffmanCode();
		ha.printCode();
	}
}
/**
 *  frequency = {12,40,15,8,25};
	5 0 0 
	8 0 0 
	6 0 0 
	5 0 0 
	7 0 0 
	6 3 0 
	7 2 5 
	8 4 6 
	0 1 7 
	a: 1111
	b: 0
	c: 011
	d: 0111
	e: 01
	
 *	####################################
 *  frequency = {45,13,12,16,9,5};
	10 0 0 
	7 0 0 
	7 0 0 
	8 0 0 
	6 0 0 
	6 0 0 
	8 5 4 
	9 2 1 
	9 6 3 
	10 7 8 
	0 0 9 
	a: 0
	b: 101
	c: 001
	d: 111
	e: 1011
	f: 0011
 **/
