import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Similarity {

    public Similarity() throws IOException {

    }
    public static void calculateSimilarity(String pathToDataCsv, String outPath) throws IOException {

        Matrix dane = new Matrix(pathToDataCsv);
        FileWriter writer = new FileWriter(outPath);

        List<Integer> selected = new ArrayList<>();
        selected.add(1);
        selected.add(318);
        selected.add(356);
        selected.add(296);
        selected.add(593);
        selected.add(2571);
        selected.add(260);
        selected.add(480);
        selected.add(527);
        selected.add(110);
        selected.add(2959);
        selected.add(72998);
        selected.add(89745);
        selected.add(589);
        selected.add(1198);
        selected.add(50);
        selected.add(4993);
        selected.add(858);
        selected.add(2858);
        selected.add(780);
        selected.add(150);
        selected.add(47);
        selected.add(3578);
        selected.add(344);
        selected.add(1193);
        selected.add(1265);
        selected.add(1036);
        selected.add(1214);
        selected.add(6874);
        selected.add(10);

        writer.append("-");
        for (int i = 0; i < selected.size(); i++) {
            writer.append(",");
            writer.append(String.valueOf(selected.get(i)));
        }

        writer.append("\n");
        int d = 1 ;
        for (Map.Entry entry : dane.dane.entrySet()) {
            writer.append(String.valueOf(entry.getKey()));
            for (int i = 0; i < selected.size(); i++) {
                double dotProduct = 0.0;
                double normA = 0.0;
                double normB = 0.0;

                for (Map.Entry entry1 : dane.dane.get(Integer.valueOf(selected.get(i))).entrySet()) {

                    if( ((HashMap<Integer,String>)entry.getValue()).containsKey(entry1.getKey())){
                        double val1 =  Double.valueOf(((HashMap<Integer,String>)entry.getValue()).get(entry1.getKey()));
                        double val2 =  Double.valueOf((String) entry1.getValue());
                        dotProduct += val1 * val2;
                        normA += Math.pow(val1, 2);
                        normB += Math.pow(val2, 2);
                    }

                }
                double similarity = dotProduct / (Math.sqrt(normA) * Math.sqrt(normB));
                d +=1;
                System.out.println(d);

                writer.append(",");
                writer.append(String.valueOf(similarity));

            }
            writer.append("\n");

        }

    }
}