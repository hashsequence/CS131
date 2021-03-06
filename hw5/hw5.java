/* Name: Avery Wong

   UID: 904582269

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*/

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.*;
import java.util.concurrent.*;

// a marker for code that you need to implement
class ImplementMe extends RuntimeException {}

// an RGB triple
class RGB {
    public int R, G, B;

    RGB(int r, int g, int b) {
    	R = r;
		G = g;
		B = b;
    }

    public String toString() { return "(" + R + "," + G + "," + B + ")"; }

}


// an object representing a single PPM image
class PPMImage {
    protected int width, height, maxColorVal;
    protected RGB[] pixels;

    public PPMImage(int w, int h, int m, RGB[] p) {
		width = w;
		height = h;
		maxColorVal = m;
		pixels = p;
    }

    // parse a PPM image file named fname and produce a new PPMImage object
    public PPMImage(String fname) 
    	throws FileNotFoundException, IOException {
		FileInputStream is = new FileInputStream(fname);
		BufferedReader br = new BufferedReader(new InputStreamReader(is));
		br.readLine(); // read the P6
		String[] dims = br.readLine().split(" "); // read width and height
		int width = Integer.parseInt(dims[0]);
		int height = Integer.parseInt(dims[1]);
		int max = Integer.parseInt(br.readLine()); // read max color value
		br.close();

		is = new FileInputStream(fname);
	    // skip the first three lines
		int newlines = 0;
		while (newlines < 3) {
	    	int b = is.read();
	    	if (b == 10)
				newlines++;
		}

		int MASK = 0xff;
		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
        is.read(bytes);
		RGB[] pixels = new RGB[numpixels];
		for (int i = 0; i < numpixels; i++) {
	    	int offset = i * 3;
	    	pixels[i] = new RGB(bytes[offset] & MASK, 
	    						bytes[offset+1] & MASK, 
	    						bytes[offset+2] & MASK);
		}
		is.close();

		this.width = width;
		this.height = height;
		this.maxColorVal = max;
		this.pixels = pixels;
    }

	// write a PPMImage object to a file named fname
    public void toFile(String fname) throws IOException {
		FileOutputStream os = new FileOutputStream(fname);

		String header = "P6\n" + width + " " + height + "\n" 
						+ maxColorVal + "\n";
		os.write(header.getBytes());

		int numpixels = width * height;
		byte[] bytes = new byte[numpixels * 3];
		int i = 0;
		for (RGB rgb : pixels) {
	    	bytes[i] = (byte) rgb.R;
	    	bytes[i+1] = (byte) rgb.G;
	    	bytes[i+2] = (byte) rgb.B;
	    	i += 3;
		}
		os.write(bytes);
		os.close();
    }

	// implement using Java 8 Streams
    public PPMImage negate() {
	//throw new ImplementMe();
	RGB[] temp = Arrays.stream(pixels).parallel().map(v -> new RGB(maxColorVal-v.R,maxColorVal-v.G,maxColorVal-v.B)).toArray(RGB[]::new);
	return new PPMImage(width, height,maxColorVal,temp);
    }

	// implement using Java 8 Streams
    public PPMImage greyscale() {
	//throw new ImplementMe();
	RGB[] temp = Arrays.stream(pixels).parallel().map(v -> new RGB((int)Math.round(.299 * v.R + .587 * v.G + .114 * v.B),
								       (int)Math.round(.299 * v.R + .587 * v.G + .114 * v.B),
								       (int)Math.round(.299 * v.R + .587 * v.G + .114 * v.B))).toArray(RGB[]::new);
	return new PPMImage(width, height,maxColorVal,temp);
    }    
    
	// implement using Java's Fork/Join library
    public PPMImage mirrorImage() {
	//throw new ImplementMe();
			
	class mirrorImageTask extends RecursiveTask<RGB[]> {
	    protected int m_width, m_height, m_up, m_down;
	    protected RGB[] m_pixels, m_pixels2;
	    protected int CUTOFF;
	    
	    
	    public mirrorImageTask(int w, int h, int u, int d, RGB[] p, RGB[] p2) {
		m_width = w;
		m_height = h;
		m_up = u;
		m_down = d;
		m_pixels = p;
		m_pixels2 = p2;
		CUTOFF = 100; // set CUTOFF = h to remove parrallelism
		
	    }
	    
	    protected RGB[] compute(){
		//RGB[] m_pixels2 = new RGB[(int)(m_width*m_height)];
		if ((m_down - m_up) <= CUTOFF) {
		 for(int i = m_up; i < m_down; i++)
		     {
			 for (int j = 0; j < m_width/2; j++)
			     {
				 int index1 = (int)(j+i*m_width);
				 int index2 = (int)(m_width - 1 - j + i*m_width);
				 m_pixels2[index2] = new RGB(m_pixels[index1].R,m_pixels[index1].G,m_pixels[index1].B);
				 m_pixels2[index1] = new RGB(m_pixels[index2].R,m_pixels[index2].G,m_pixels[index2].B);				 
				 
			     }
		     }
		 return m_pixels2;
		}
		int mid = (m_up + m_down)/2;
		//		int mid = (m_down - m_up) / 2 + m_up;
		mirrorImageTask Upper = new mirrorImageTask(m_width,m_height,m_up,mid,m_pixels,m_pixels2);
		mirrorImageTask Lower = new mirrorImageTask(m_width,m_height,mid,m_down,m_pixels,m_pixels2);
		Upper.fork();
		Lower.compute();
		Upper.join();
		return m_pixels2;
		
			    
	    }
	}
	    RGB[] pixels2 = new RGB[(int)(width*height)]; //holds the mirror array
	    mirrorImageTask PPMITask = new mirrorImageTask(width, height,0, height ,pixels,pixels2);
	    RGB[] temp = PPMITask.compute();
	    return new PPMImage(width, height, maxColorVal, temp);
    }

	// implement using Java 8 Streams
    public PPMImage mirrorImage2() {

		RGB[] pixels2 = new RGB[(int)(width*height)]; //holds the mirror array
		IntStream.range(0, height).parallel().forEach(i ->
									  {
									      /*
									      IntStream.range(0, width/2).forEach(j -> {
									      int index1 = (int)(j+i*width);
									      int index2 = (int)(width - 1 - j + i*width);
									      pixels2[index2] = new RGB(pixels[index1].R,pixels[index1].G,pixels[index1].B);
								              pixels2[index1] = new RGB(pixels[index2].R,pixels[index2].G,pixels[index2].B);
										  });
									      */
									      
									      for (int j = 0; j < width/2; j++)
										  {
										      int index1 = (int)(j+i*width);
										      int index2 = (int)(width - 1 - j + i*width);
										      pixels2[index2] = new RGB(pixels[index1].R,pixels[index1].G,pixels[index1].B);
										      pixels2[index1] = new RGB(pixels[index2].R,pixels[index2].G,pixels[index2].B);

										  }
									      
									  });
		    return new PPMImage(width, height, maxColorVal, pixels2);
    }

	// implement using Java's Fork/Join library
    public PPMImage gaussianBlur(int radius, double sigma) {
	//	throw new ImplementMe();
	
		class gTask extends RecursiveTask<RGB[]> {
		    
		    protected double[][] m_gf;
		    protected RGB[] m_pa;
		    protected RGB[] m_pa2;
		    protected int m_width;
		    protected int m_height;
		    protected int m_rad;
		    protected int CUTOFF; 
		    protected int m_up;
		    protected int m_down;
		    
		    public gTask(double[][]gf, RGB[] pa, RGB[] pa2,  int width, int height, int rad, int up, int down)
		    {
			m_gf = gf;
			m_pa = pa;
			m_width = width;
			m_height = height;
			m_rad = rad;	
			m_pa2 = pa2;
			CUTOFF = 10000; // set CUTOFF = height to remove parrallelism
			m_up = up;
			m_down = down;
			
		    }
		    
		    
		    protected RGB[] compute()
		    {
			//if(true){
			/*
			if ((m_down - m_up) <= CUTOFF) {
			for (int i = m_up; i < m_down; i++)
			    {
				for (int j = 0; j < m_width; j++)
				    {
					m_pa2[j + i*m_width] = filterPixel(m_pa,i,j,m_width,m_height,m_gf,m_rad);
				    }
			    }
			return m_pa2;
		     }
			*/

			if((m_down - m_up) <= CUTOFF){
			    for (int i = m_up; i < m_down; i++)
				{
				    int k = i / m_width;
				    int j = i - m_width*k;
				    m_pa2[i] = filterPixel(m_pa,k,j,m_width,m_height,m_gf,m_rad);
				}
			    return m_pa2;
			}
			/*
		        int mid1 = (m_down - m_up)/4 + m_up;
			int mid2 = (m_down - m_up)/4 * 2 + m_up;
			int mid3 = (m_down - m_up)/4 * 3 + m_up;
			gTask Upper = new gTask(m_gf, m_pa, m_pa2, m_width, m_height, m_rad, m_up, mid1);
			gTask Middle1 = new gTask(m_gf, m_pa, m_pa2, m_width, m_height, m_rad, mid1, mid2);
			gTask Middle2 = new gTask(m_gf, m_pa, m_pa2, m_width, m_height, m_rad, mid2, mid3);
			gTask Lower = new gTask(m_gf, m_pa, m_pa2, m_width, m_height, m_rad, mid2, m_down);
			Upper.fork();
			Middle1.fork();
			Middle2.fork();
			Lower.compute();
			Upper.join();
			Middle1.join();
			Middle2.join();
			*/
			int mid = (m_down + m_up)/2;
			//int mid = (m_down - m_up)/2 + m_up;
			gTask Upper = new gTask(m_gf, m_pa, m_pa2, m_width, m_height, m_rad, m_up, mid);
			gTask Lower = new gTask(m_gf, m_pa, m_pa2, m_width, m_height, m_rad, mid, m_down);
			Upper.fork();
			Lower.compute();
			Upper.join();
			return m_pa2;
		    }

		    protected RGB filterPixel(RGB[] pa, int i, int j, int width, int height, double[][] gf, int rad)
		    {
			int i_1 = i - rad;
			int j_1 = j - rad;
			//			System.out.println(" i_1: " + new Integer(i_1).toString() + " j_1: " + new Integer(j_1).toString());
			double r = 0;
			double g = 0;
			double b = 0;
			//			try {
			for (int k = 0; k < 2*rad+1; k++)
			    {
				for (int l = 0; l < 2*rad+1; l++)
				    {
					
					if (i_1 <0 && j_1 < 0)
					    {
						r+=gf[k][l]*pa[0].R;
						g+=gf[k][l]*pa[0].G;
						b+=gf[k][l]*pa[0].B;
						
					    }
					else if(j_1 >= width && i_1 >= height)
					    {
					
						r+=gf[k][l]*pa[(width) * (height) - 1].R;
						g+=gf[k][l]*pa[(width) * (height) - 1].G;
						b+=gf[k][l]*pa[(width) * (height) - 1].B;
						
					    }
					else if(j_1 < 0 && i_1 >= height)
					    {
						r+=gf[k][l]*pa[(height - 1)*width].R;
						g+=gf[k][l]*pa[(height - 1)*width].G;
						b+=gf[k][l]*pa[(height - 1)*width].B;	
					    }
					else if(j_1 >= width  && i_1 < 0)
					    {
						r+=gf[k][l]*pa[width-1].R;
						g+=gf[k][l]*pa[width-1].G;
						b+=gf[k][l]*pa[width-1].B;	
					    }
					else if(j_1 < 0)
					    {
						r+=gf[k][l]*pa[i_1 * width].R;
						g+=gf[k][l]*pa[i_1 * width].G;
						b+=gf[k][l]*pa[i_1 * width].B;
						
					    }
					else if(i_1 < 0)
					    {
					
						r+=gf[k][l]*pa[j_1].R;
						g+=gf[k][l]*pa[j_1].G;
						b+=gf[k][l]*pa[j_1].B;
						
					    }
					else if(j_1 >= width)
					    {
					
						r+=gf[k][l]*pa[(width-1) + i_1*width].R;
						g+=gf[k][l]*pa[(width-1) + i_1*width].G;
						b+=gf[k][l]*pa[(width-1) + i_1*width].B;
						
					    }
					else if(i_1 >= height)
					    {

						r+=gf[k][l]*pa[j_1 + (height-1)*width].R;
						g+=gf[k][l]*pa[j_1 + (height-1)*width].G;
						b+=gf[k][l]*pa[j_1 + (height-1)*width].B;
						
					    }
					else
					    {

						r+=gf[k][l]*pa[j_1+i_1*width].R;
						g+=gf[k][l]*pa[j_1+i_1*width].G;
						b+=gf[k][l]*pa[j_1+i_1*width].B;
						
					    }
					j_1++;
				    }
			        j_1 = j-rad;
				i_1++;
			    }
			/*
			} catch(Exception e) {
			    System.out.println("You done broke it: " + e.toString() + " i_1: " + new Integer(i_1).toString() +" j_1: " + new Integer(j_1).toString());
			    java.lang.System.exit(0);
			}
			*/
			
			return new RGB((int)r,(int)g,(int)b);
		    }
		    
		}
		RGB[] t_pa =  new RGB[(int)(width*height)]; //holds the output array
		double[][] gf = Gaussian.gaussianFilter(radius, sigma);
		gTask t = new gTask(gf, pixels, t_pa, width, height, radius,0,height*width);
		t.compute();

		return new PPMImage(width, height, maxColorVal, t_pa);
	
    }



    
}


// code for creating a Gaussian filter
class Gaussian {

    protected static double gaussian(int x, int mu, double sigma) {
		return Math.exp( -(Math.pow((x-mu)/sigma,2.0))/2.0 );
    }

    public static double[][] gaussianFilter(int radius, double sigma) {
		int length = 2 * radius + 1;
		double[] hkernel = new double[length];
		for(int i=0; i < length; i++)
	    	hkernel[i] = gaussian(i, radius, sigma);
		double[][] kernel2d = new double[length][length];
		double kernelsum = 0.0;
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++) {
				double elem = hkernel[i] * hkernel[j];
				kernelsum += elem;
				kernel2d[i][j] = elem;
	    	}
		}
		for(int i=0; i < length; i++) {
	    	for(int j=0; j < length; j++)
				kernel2d[i][j] /= kernelsum;
		}
		return kernel2d;
    }
}
/*
class test {
    public static void main(String [] args) {
        try {
			long t0, t1;
			PPMImage ppm = new PPMImage(args[0]);

			System.out.println("time tests");
			System.out.println("Negating");
			t0 = System.nanoTime();
			PPMImage neg = ppm.negate();
			t1 = System.nanoTime();
			System.out.println( (t1-t0)/1000000 + " ms");

			neg.toFile("neg.ppm");

			System.out.println("Greyscaling");
			t0 = System.nanoTime();
			PPMImage grey = ppm.greyscale();
			t1 = System.nanoTime();
			System.out.println( (t1-t0)/1000000 + " ms");

			grey.toFile("grey.ppm");
				    			
			System.out.println("Mirroring");
			t0 = System.nanoTime();
			PPMImage mir = ppm.mirrorImage();
			t1 = System.nanoTime();
			System.out.println( (t1-t0)/1000000 + " ms");
			
			mir.toFile("mirror.ppm");

			
			System.out.println("Mirroring2");
			t0 = System.nanoTime();
			PPMImage mir2 = ppm.mirrorImage2();
			t1 = System.nanoTime();
			System.out.println( (t1-t0)/1000000 + " ms");

			mir2.toFile("mirror2.ppm");



			System.out.println("Blurring (60, 120.0)");
			t0 = System.nanoTime();
			PPMImage blur = ppm.gaussianBlur(60, 120.0);
			t1 = System.nanoTime();
			System.out.println( (t1-t0)/1000000 + " ms");

			blur.toFile("blur.ppm");

			System.out.println("Blurring (30, 60.0)");
			t0 = System.nanoTime();
			PPMImage blur2 = ppm.gaussianBlur(30, 60.0);
			t1 = System.nanoTime();
			System.out.println( (t1-t0)/1000000 + " ms");

			blur2.toFile("blur2.ppm");
			
 			System.out.println("Blurring (5, 10.0)");
			t0 = System.nanoTime();
			PPMImage blur3 = ppm.gaussianBlur(5, 10.0);
			t1 = System.nanoTime();
			System.out.println( (t1-t0)/1000000 + " ms");

			blur3.toFile("blur3.ppm");

			System.out.println("Blurring (1, 2.0)");
			t0 = System.nanoTime();
			PPMImage blur4 = ppm.gaussianBlur(1, 2.0);
			t1 = System.nanoTime();
			System.out.println( (t1-t0)/1000000 + " ms");

			blur4.toFile("blur4.ppm");
						
        } catch(IOException e) {
            System.out.println("IOException");
        } catch(Exception e) {
            System.out.println(e.toString());
        }
    }
}

*/
