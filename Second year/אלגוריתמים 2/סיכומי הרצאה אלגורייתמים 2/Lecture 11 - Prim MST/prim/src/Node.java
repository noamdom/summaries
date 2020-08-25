public class Node{
		int key;// the key of the vertex for adjacency table 
		int vertex; // the vertex number
		final int WHITE = 0, NIL = -1;
		public Node(int v, int key){
			this.key = key;
			this.vertex = v;
		}
		public Node(){
			this.key = 0;
			this.vertex = -1;
		}
		public Node(Node node) {
			this(node.vertex, node.key);
		}
		public String toString(){
			//return "(w: " + key + ", "+vertex + ")";
			return "(" + vertex+ ", w: " + key + ")";
		}
	}
