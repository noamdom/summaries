import java.util.ArrayList;

public class Prim2 {
	HeapMin Q;
	ArrayList<Node>[] graph;
	int [] key, parents;
	boolean [] visited;
	int numOfVertexes, nEdges, root;
	final int infinity = Integer.MAX_VALUE, NIL = -1;
	Edge T[];
	public Prim2(ArrayList<Node>[] G, int root){
		graph = G;
		numOfVertexes = graph.length;
		this.root = root;
		key = new int[numOfVertexes];
		parents = new int[numOfVertexes];
		visited = new boolean[numOfVertexes];
		T = new Edge[numOfVertexes-1];
		nEdges =  0;
		for (int i=0; i<numOfVertexes; i++) {
			key[i] = infinity;
			parents[i] = NIL;
			visited[i] = false;
		}
		key[root] = 0;
		Q = new HeapMin();
		Q.minHeapInsert(new Node(root,0));
		for (int i=1; i<numOfVertexes; i++){
			Q.minHeapInsert(new Node(i,infinity));
		}
	}
	public void MST_Prim(){
		while(!Q.isEmpty() && nEdges<numOfVertexes-1){
			int u = Q.heapExtractMin().vertex;
			for (int i=0; i<graph[u].size(); i++){
				Node n = graph[u].get(i);
				int v = n.vertex;
				int uvWeight = n.key;
				if (!visited[v] &&  uvWeight < key[v]){
					parents[v] = u;
					key[v] = uvWeight;
					Q.heapDecreaseKey(n);
				}
			}
			visited[u] = true;
			Node x =  Q.heapGetMin();
			T[nEdges++] = new Edge(parents[x.vertex], x.vertex, x.key);
		}
	}
	void PrintMST(){
		int sumWeight = 0;
		for (int i = 0; i < key.length; i++) {
			sumWeight = sumWeight +key[i];
		}
		System.out.println("MSP Prim Summary Weight: "+sumWeight);
		System.out.print("MSP Prim Weights :  ");
		for (int i = 0; i < key.length; i++) {
			System.out.print((key[i])+", ");
		}
		System.out.println();
		System.out.print("MSP Prim parents :  ");
		for (int i = 0; i < parents.length; i++) {
			System.out.print((parents[i])+", ");
		}
		System.out.println();
		for (int i = 0; i < T.length; i++) {
			System.out.print(T[i] + ", ");
		}
	}
	@SuppressWarnings("unchecked")
	public static ArrayList<Node>[] init1(){
		int numOfVertexes = 9;
		ArrayList<Node>[] graph = new ArrayList[numOfVertexes];
		for (int i=0; i<numOfVertexes; i++) {
			graph[i] = new ArrayList<Node>();
		}
		// vertex: (adjacency vertex, key) 
		// a - first vertex:
		graph[0].add(new Node(1,4)); graph[0].add(new Node(7,8)); 
		// b - second vertex:
		graph[1].add(new Node(0,4)); graph[1].add(new Node(2,8)); graph[1].add(new Node(7,11)); 
		// c - third vertex:
		graph[2].add(new Node(1,8)); graph[2].add(new Node(3,7)); graph[2].add(new Node(5,4)); graph[2].add(new Node(8,2)); 
		// d - 4-th vertex:
		graph[3].add(new Node(2,7)); graph[3].add(new Node(4,9)); graph[3].add(new Node(5,14)); 
		// e - 5-th vertex:
		graph[4].add(new Node(3,9)); graph[4].add(new Node(5,10)); 
		// f - 6-th vertex:
		graph[5].add(new Node(2,4)); graph[5].add(new Node(3,14)); graph[5].add(new Node(4,10)); graph[5].add(new Node(6,2)); 
		// f - 7-th vertex:
		graph[6].add(new Node(5,2)); graph[6].add(new Node(7,1)); graph[6].add(new Node(8,6)); 
		// f - 8-th vertex:
		graph[7].add(new Node(0,8)); graph[7].add(new Node(1,11)); graph[7].add(new Node(6,1)); graph[7].add(new Node(8,7)); 
		// f - 9-th vertex:
		graph[8].add(new Node(2,2)); graph[8].add(new Node(6,6)); graph[8].add(new Node(7,7)); 
		return graph;
	}
	@SuppressWarnings("unchecked")
	public static ArrayList<Node>[] init2(){
		int numOfVertexes = 3;
		ArrayList<Node>[]graph = new ArrayList[numOfVertexes];
		for (int i=0; i<numOfVertexes; i++) {
			graph[i] = new ArrayList<Node>();
		}
		// vertex: (adjacency vertex, key) 
		// a - first vertex:
		graph[0].add(new Node(1,3)); graph[0].add(new Node(2,4)); 
		// b - second vertex:
		graph[1].add(new Node(0,3)); graph[1].add(new Node(2,4)); 
		// c - third vertex:
		graph[2].add(new Node(0,4)); graph[2].add(new Node(1,4)); 
		return graph;
	}
	@SuppressWarnings("unchecked")
	public static ArrayList<Node>[] init3(){
		int numOfVertexes = 4;
		ArrayList<Node>[] graph = new ArrayList[numOfVertexes];
		for (int i=0; i<numOfVertexes; i++) {
			graph[i] = new ArrayList<Node>();
		}
		graph[0].add(new Node(1,5)); graph[0].add(new Node(2,4)); graph[0].add(new Node(3,2));
		graph[1].add(new Node(0,5)); graph[1].add(new Node(3,1));
		graph[2].add(new Node(0,4));
		graph[3].add(new Node(0,2));  graph[3].add(new Node(1,1)); 
		// vertex: (adjacency vertex, key) 
		return graph;
	}
	public static void main(String[] args) {
		Prim2 p = new Prim2(init1(), 0);
		p.MST_Prim();
		p.PrintMST();

	}

}
/**	init1
 MSP Prim Summary Weight: 37
 MSP Prim Weights :  0, 4, 4, 7, 9, 2, 1, 8, 2, 
 MSP Prim parents :  -1, 0, 5, 2, 3, 6, 7, 0, 2, 
 (0,1,w:4), (0,7,w:8), (7,6,w:1), (6,5,w:2), (5,2,w:4), (2,8,w:2), (2,3,w:7), (3,4,w:9),  
 	init2
 MSP Prim Summary Weight: 7
MSP Prim Weights :  0, 3, 4, 
MSP Prim parents :  -1, 0, 0, 
(0,1,w:3), (0,2,w:4), 
 	init3
MSP Prim Summary Weight: 7
MSP Prim Weights :  0, 1, 4, 2, 
MSP Prim parents :  -1, 3, 0, 0, 
(0,3,w:2), (3,1,w:1), (0,2,w:4), */
