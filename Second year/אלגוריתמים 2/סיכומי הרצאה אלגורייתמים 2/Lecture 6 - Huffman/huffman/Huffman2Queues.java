package huffman;

import java.util.concurrent.ArrayBlockingQueue;


public class Huffman2Queues {
	private Node nodes[];
	private int numOfLeaves, numNodes, place;
	private final int nil = -1;
	private String codes[];
	private char letters[];
	ArrayBlockingQueue<Node> q1, q2;
//array of frequencies is sorted: f[0]<f[1]<...<f[n-1]
	public Huffman2Queues(int[] freq, char[] letters) {
		numOfLeaves = freq.length;
		numNodes = numOfLeaves*2-1;
		nodes = new Node[numNodes];
		codes = new String[numOfLeaves];
		this.letters = new char[numOfLeaves];
		place = numOfLeaves;
		q1 = new ArrayBlockingQueue<Node>(numOfLeaves);
		q2 = new ArrayBlockingQueue<Node>(numOfLeaves);
		for (int i=0; i<numOfLeaves; i++){//O(n)
			nodes[i] = new Node(i,freq[i]);
			q1.add(nodes[i]); 
			codes[i] = new String();
			this.letters[i] = letters[i];
		}
	}
	public void huffmanON() {
		while (q1.size() + q2.size() > 1 ) {
			Node n1 = nextMin();//O(1)
			Node n2 = nextMin();
			Node newNode = new Node(place, n1.key+n2.key, n1.letterNumber, n2.letterNumber);
			n1.setParent(place);
			n2.setParent(place);//O(1)
			q2.add(newNode);//O(1)
			nodes[place] = newNode;
			place++;
		}
		//// build the Huffman's Code for all letters
		for (int i=0; i<numOfLeaves; i++){//O(2n-1)
			Node child = nodes[i];
			Node parent = nodes[child.parent];
			while(child.parent != nil){
				if (parent.left==child.letterNumber) codes[i] = "0" + codes[i];
				else codes[i] = "1" + codes[i];	
				child = parent;
				if (child.parent != nil) parent = nodes[child.parent];
			}
		}
	}
	private Node nextMin(){
		Node x, y;
		if (q1.isEmpty())	x = q2.poll();  
		else if (q2.isEmpty()) x = q1.poll();  
		else{
			x = q1.peek();
			y = q2.peek();
			if (x.key > y.key) x = q2.poll();
			else x = q1.poll();
		}
		return x;
	}
	public void printCode(){
		for (int i=0; i<numOfLeaves; i++){
			System.out.print(letters[i]+": "+codes[i] + ";  ");
		}
		System.out.println();
	}
	public static void main(String[] args) {
		int freq1[] = {5,9,12,13,16,45};
		int freq2[] = {8,12,15,25,40};
		char letter1[] = {'f','e','c','b','d','a'};
		char letter2[] = {'d','a','c','e','b'};
		Huffman2Queues hf2 = new Huffman2Queues(freq1, letter1);
		hf2.huffmanON();
		hf2.printCode();
		hf2 = new Huffman2Queues(freq2, letter2);
		hf2.huffmanON();
		hf2.printCode();	
	}

/* 	f: 1100;  e: 1101;  c: 100;  b: 101;  d: 111;  a: 0;  
	d: 1110;  a: 1111;  c: 110;  e: 10;  b: 0;  
*/
}
