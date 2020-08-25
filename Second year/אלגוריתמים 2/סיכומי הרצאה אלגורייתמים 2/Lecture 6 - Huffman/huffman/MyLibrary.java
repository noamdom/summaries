package huffman;

public class MyLibrary {
/**
 * print character array
 */
	public static void printCharArray(char [] arr){
		for(int t=0; t<arr.length; t++){
			System.out.print(arr[t]+", ");
		}
		System.out.println();
	}
	/**
	 * print integer array
	 */
	public static void printIntegerArray(int [] arr){
		for(int t=0; t<arr.length; t++){
			System.out.print(arr[t]+", ");
		}
		System.out.println();
	}
	/**
	 * print matrix
	 */
	public static void printIntMatrix(int [][] mat){
		for(int i=0; i<mat.length; i++){
			for(int j=0; j<mat[0].length; j++){
				System.out.print(mat[i][j]+", ");
			}
			System.out.println();
		}
		System.out.println();
	}
	/**
	 * print double array
	 */
	public static void printDoubleArray(double [] arr){
		for(int t=0; t<arr.length; t++){
			System.out.print(arr[t]+", ");
		}
		System.out.println();
	}
	/**
	 * returns character array of given size; 
	 * random characters are the first numOfLetters letters of 
	 * the alphabet
	 */
	public static char [] randomCharArray(int size, int numOfLetters){
		char []arr = new char[size];
		for(int i=0; i<arr.length; i++){
			int x = (int)(Math.random()*numOfLetters+'a');
			arr[i] = (char)x;
		}
		return arr;
	}
	
	/**
	 * returns character array of given size; 
	 * random characters are the first numOfLetters letters of 
	 * the alphabet
	 */
	public static String randomString(int size, int numOfLetters){
		String ans = "";
		for(int i=0; i<size; i++){
			char x = (char) (Math.random()*numOfLetters+'a');
			ans = ans + x;
		}
		return ans;
	}
	
	/**
	 * returns integer array of given size; 
	 * random number are into the interval [0,size-1] 
	 */
	public static int [] randomIntegerArray(int size){
		int []arr = new int[size];
		for(int i=0; i<arr.length; i++){
			arr[i] = (int)(Math.random()*size);
		}
		return arr;
	}
	/**
	 * returns integer array of given size; 
	 * random number are into the interval [0,size-1] 
	 */
	public static int [] randomIntegerArrayMinus(int size){
		int []arr = new int[size];
		for(int i=0; i<arr.length; i++){
			arr[i] = (int)(Math.random()*2*size) - size;
		}
		return arr;
	}
	public static int [] randomIntArrayOfDiffNumbers(int size){
		int []arr = new int[size];
		for(int i=0; i<arr.length;){
			int randNumber = (int)(Math.random()*size);
			if (!contains(arr,i-1,randNumber)){
				arr[i] = randNumber;
				i++;
			}
		}
		return arr;
	}
	public static boolean contains(int [] arr, int end, int value){
		boolean ans = false;
		for(int i=0; !ans &&i<=end; i++){
			if (arr[i]==value) ans = true;
		}
		return ans;
	}
	/**
	 * Binary search by induction
	 * @param arr - sorted array of integer numbers
	 * @param value to search
	 * @return index of value if found, otherwise return -1
	 */
	public static int binarySearchInducion(int []arr,int value){
		int low = 0, high = arr.length-1;
		while (low <=high){
			int middle = (low + high)/2;
			if (low == high) {
				if (arr[low] == value) return low;
				else return -1;
			}
			else {
				if (arr[middle] == value){//value was found
					return middle;
				}
				// value suppose to be left
				if (value < arr[middle]){
					high = middle;
				}
	 			// value suppose to be right
				else{
					low = middle+1;
				}
			}
		}
		return -1;
	}
	public static int[][] randomMatrixOf01(int rows, int cols){
		int mat[][] = new int[rows][cols];
		for(int i=0; i<rows; i++){
			for(int j=0; j<cols; j++){
				mat[i][j] = (int)(Math.random()*2);
			}
		}
		return mat;
	}
	public static int[][] randomMatrix(int rows, int cols){
		int mat[][] = new int[rows][cols];
		for(int i=0; i<rows; i++){
			for(int j=0; j<cols; j++){
				mat[i][j] = (int)(Math.random()*(rows+cols));
			}
		}
		return mat;
	}
	public static int[][][] randomMatrix3D(int n){
		int mat[][][] = new int[n][n][n];
		for(int i=0; i<n; i++){
			for(int j=0; j<n; j++){
				for(int k=0; k<n; k++){
					mat[i][j][k] = (int)(Math.random()*2*n);
				}
			}
		}
		return mat;
	}
	public static int[][] matrixSqMulti(int[][] m1, int m2[][]){
		int len=m1.length;
		int res[][] = new int[len][len];
		for(int i=0; i<len; i++){
			for(int j=0; j<len; j++){
				res[i][j] = 0;
				for(int k=0; k<len; k++){
					res[i][j] = res[i][j] + m1[i][k]*m2[k][j]; 
				}
			}
		}
		return res;
	}
	public static void main(String[] args) {
		int mat1[][] = {{1,1},{1,0}};
		int mat2[][] = {{3,2},{2,1}};
		int res[][] = matrixSqMulti(mat1, mat2);
		printIntMatrix(res);		
		//printIntegerArray(randomIntArrayOfDiffNumbers(6));
		System.out.println(randomString(6, 4));
	}
}
