import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class Matrix {
    String PTCSV;
    HashMap<Integer,HashMap<Integer,String>> dane;

    public Matrix(String PTCSV) throws IOException {
        HashMap<Integer,HashMap<Integer,String>> dane = new HashMap<Integer,HashMap<Integer,String>>();
        BufferedReader br1 = new BufferedReader(new FileReader(PTCSV));
        String line1 = null;
        line1 = br1.readLine();
        int j = 1;
        while ((line1 = br1.readLine()) != null) {
            j += 1 ;
            if (j % 1000 == 0) {
                System.out.println(j);
            }


            String[] row2 = line1.split(",");

            if (dane.containsKey(Integer.valueOf(row2[2]))) {
                dane.get(Integer.valueOf(row2[2])).put(Integer.valueOf(row2[1]), row2[3]);
            } else {
                HashMap<Integer, String> pomoc = new HashMap<Integer, String>();
                pomoc.put(Integer.valueOf(row2[1]), row2[3]);
                dane.put(Integer.valueOf(row2[2]), pomoc);
            }
        }
        this.dane = dane;
    }


}