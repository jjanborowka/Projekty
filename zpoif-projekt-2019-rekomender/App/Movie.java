import javax.swing.*;

public class Movie {
    ImageIcon poster;
    String title;
    String id;
    double score;

    public String getId() {
        return id;
    }

    public Movie(ImageIcon poster, String title, String id, double score) {
        this.poster = poster;
        this.title = title;
        this.id = id;
        this.score = score;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public void setScore(double score) {
        this.score = score;
    }

    public String getTitle() {
        return title;
    }

    public double getScore() {
        return score;
    }

    public ImageIcon getPoster() {return poster;}
}
