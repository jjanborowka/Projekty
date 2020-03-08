import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

public class Main {

    public static void main(String[] args) throws IOException{

        //Working directory
        String wd = System.getProperty("user.dir");

        //Calculation of similarity matrix if needed
        File similarityMatrixFile = new File(wd + "/data/similarityMatrix.csv");

        if (!similarityMatrixFile.exists()) {
            //List of movies' ids which are scored by user
            ArrayList<Integer> userScoredMovies = new ArrayList<>();

            userScoredMovies.add(318);
            userScoredMovies.add(356);
            userScoredMovies.add(296);
            userScoredMovies.add(593);
            userScoredMovies.add(2571);
            userScoredMovies.add(260);
            userScoredMovies.add(480);
            userScoredMovies.add(527);
            userScoredMovies.add(110);
            userScoredMovies.add(1);
            userScoredMovies.add(2959);
            userScoredMovies.add(72998);
            userScoredMovies.add(89745);
            userScoredMovies.add(589);
            userScoredMovies.add(1198);
            userScoredMovies.add(50);
            userScoredMovies.add(4993);
            userScoredMovies.add(858);
            userScoredMovies.add(2858);
            userScoredMovies.add(780);
            userScoredMovies.add(150);
            userScoredMovies.add(47);
            userScoredMovies.add(3578);
            userScoredMovies.add(344);
            userScoredMovies.add(1193);
            userScoredMovies.add(1265);
            userScoredMovies.add(1036);
            userScoredMovies.add(1214);
            userScoredMovies.add(6874);
            userScoredMovies.add(10);

            //Path to data base copy csv
            Similarity.calculateSimilarity(wd + "/data/ratings.csv", wd + "/data/similarityMatrix.csv");

        }

        //Running GUI
        GUI.init();
    }

}
