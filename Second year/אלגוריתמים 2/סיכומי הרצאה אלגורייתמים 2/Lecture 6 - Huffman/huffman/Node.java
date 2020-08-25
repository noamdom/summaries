package huffman;

public class Node implements Comparable<Node>{
	/** @param:
	 * number - a letter number
	 * key - a letter frequency (probability)
	 */ 
	private final int nil = -1;
	int letterNumber, key, parent;
	int left, right;
	/** constructor of Node*/
	public Node(int letterNumber, int key, int left, int right){
		this.letterNumber = letterNumber;
		this.key = key;
		this.parent = nil;
		this.left = left;
		this.right = right;
	}
	public Node(int letterNumber, int key){
		this.letterNumber = letterNumber;
		this.key = key;
		this.parent = nil;
		this.left = nil;
		this.right = nil;
	}
	public void setParent(int parent){
		this.parent = parent;
	}
	/** convert parameters - to string */ 
	public String toString(){
		return "("+letterNumber+","+key+" "+","+left+","+right+","+parent +" )";
	}
	@Override
	public int compareTo(Node n) {
		int ans = this.key - n.key;
		return ans;
	}
}
