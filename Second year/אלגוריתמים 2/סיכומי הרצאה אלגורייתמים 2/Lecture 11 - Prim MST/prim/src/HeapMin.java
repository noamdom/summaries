public class HeapMin {

	int _positiveInfinity = Integer.MAX_VALUE;
	final int INIT_SIZE = 10;
	private Node _a[];
	private int _size;
	public HeapMin(Node arr[]){
		_size = arr.length;
		_a = new Node[_size];
		for (int i=0; i<_size; i++){
			_a[i]=arr[i];
		}
	}
	public HeapMin(){
		_a = new Node[0];
	}
	/** returns the heap size*/
	public int getSize(){return _size;}
	/** returns the heap array */
	public Node[] getA(){ return _a;}
	
	/** parent returns the parent of vertex  i*/
	private  int parent(int i){return (i-1)/2;}
	
	/** leftChild returns the left child of vertex  i*/
	private  int leftChild(int i){return 2*i+1;}
	/** rightChild returns the right child of vertex  i*/
	private  int rightChild(int i){return 2*i+2;}
	/** returns the heap minimum */
	public Node heapMinimum(){return _a[0];}
	/** returns true if the heap is empty, otherwise false */
	public boolean isEmpty(){
		boolean ans = false;
		if (_size == 0) ans = true;
		return ans;
	}
	/** the minHeapfy function maintains the min-heap property */
	private void minHeapify(int v, int heapSize){
		int smallest;
		int left = leftChild(v);
		int right = rightChild(v);
		if (left<heapSize && _a[left].key<_a[v].key){
			smallest = left;
		}
		else{
			smallest = v;
		}
		if (right<heapSize && _a[right].key<_a[smallest].key){
			smallest = right;
		}
		if (smallest!=v){
			exchange(v, smallest);
			minHeapify(smallest, heapSize);
		}		
	}

	/** the heap minimum element extraction */
	public Node heapExtractMin(){
		int min = _positiveInfinity; // infinity
		Node node=null;
		if (!isEmpty()){
			node = _a[0];
			min = node.key;
			_a[0]=_a[_size-1];
			_size = _size-1;
			minHeapify(0, _size);
		}
		return node;
	}
	public Node heapGetMin(){
		return _a[0];
	}
	/** the heapDecreaseKey implements the Decrease Key operation*/
	public void heapDecreaseKey(Node node){
		int v = node.vertex;
		int i = 0;
		while (i<_size && v!=_a[i].vertex) i++;
		if (node.key <_a[i].key){
			_a[i] = node;
			while (i>0 && _a[parent(i)].key>_a[i].key){
				exchange(i, parent(i));
				i = parent(i);
			}
		}
	}
	/** minHeapInsert function implements the Insert-Key operation*/
	public void minHeapInsert(Node node){//O(log2(n))
		resize(1);
		_a[_size-1] = new Node(node);
		_a[_size-1].key = _positiveInfinity;
		heapDecreaseKey(node);
	}
	/** increment an array*/
	private void resize(int increment){
		Node temp[] = new Node[_size+increment];
		for (int i=0; i<_size; i++){
			temp[i]=_a[i];
		}
		_a = temp;
		_size = _size+increment;
	}	
	/** exchange two array elements*/
	private void exchange(int i, int j){
		Node t = _a[i];
		_a[i] = _a[j];
		_a[j] = t;
	}

}
