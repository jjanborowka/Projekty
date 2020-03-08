import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableColumn;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class GUI {

    public static void init() throws IOException {

        //path to Movies.csv
        String moviesPath = System.getProperty("user.dir");
        moviesPath += "/data/Movies.csv";

        //path to similaritymatrix
        String sMatrixPath = System.getProperty("user.dir");
        sMatrixPath += "/data/similarityMatrix.csv";

        JFrame frame = new JFrame("Movie Recommender");
        frame.setSize(new Dimension(1000, 600));

        //Preparing data for algorithm and app
        MovieReader movieReader = new MovieReader();
        HashMap<String, String> allTitles = movieReader.readAllTitles(moviesPath);
        List<Movie> moviesToGUI = movieReader.readScoreMovies(moviesPath);

        //GUI

        //Top panel
        JPanel topPanel = new JPanel();
        topPanel.setBackground(new ColorUIResource(92,92,92));
        JLabel topText = new JLabel("Please rate 10 movies to receive a recommendation");
        topText.setFont(new Font("Serif", Font.BOLD, 25));
        topText.setForeground(Color.white);
        topPanel.add(topText, "Center");
        frame.getContentPane().add(topPanel, "North");

        //left panel
        JPanel leftPanel = new JPanel();
        leftPanel.setLayout(new BorderLayout());

        //Table with scores
        TableModel model = new TableModel(moviesToGUI);
        JTable table = new JTable(model);

        //Posters column
        TableColumn col0 = table.getColumnModel().getColumn(0);
        col0.setPreferredWidth(60);

        //row height
        table.setRowHeight(140);

        //column nr 2 size and input
        TableColumn col2 = table.getColumnModel().getColumn(2);
        col2.setPreferredWidth(250);
        col2.setCellEditor(new SliderEditor());
        col2.setCellRenderer(new CellRenderer());

        //column nr 1 size
        TableColumn col1 = table.getColumnModel().getColumn(1);
        col1.setPreferredWidth(400);

        //column nr 3 background
        TableColumn col3 = table.getColumnModel().getColumn(3);

        //prediction button
        JButton runPrediction = new JButton("Give prediction");
        JScrollPane forTable = new JScrollPane(table);
        runPrediction.setEnabled(false);

        //scrolling panel for table
        JScrollPane forTable1 = new JScrollPane(table);

        //Left side splitter
        JSplitPane splitLeft = new JSplitPane(JSplitPane.VERTICAL_SPLIT, forTable1, runPrediction);
        splitLeft.setResizeWeight(0.9);
        leftPanel.add(splitLeft);
        splitLeft.setPreferredSize(new Dimension(800, 30));
        splitLeft.setVisible(true);

        //right panel
        JPanel rightPanel = new JPanel();
        rightPanel.setLayout(new BorderLayout());

        //background panel for text output panel
        JPanel backOutput = new JPanel();
        backOutput.setLayout(new GridBagLayout());

        //text panel for output
        JTextPane output = new JTextPane();
        output.setFont(new Font("Serif", Font.BOLD, 25));
        Style outputText = output.addStyle("", null);
        StyleConstants.setForeground(outputText, Color.white);
        StyleConstants.setBackground(outputText, new ColorUIResource(36, 34, 34));

        output.setEditable(false);
        backOutput.setSize(600, 600);
        backOutput.add(output);
        rightPanel.add(backOutput);
        backOutput.setBackground(new ColorUIResource(92,92,92));
        output.setBackground(new ColorUIResource(92, 92, 92));

        // Centering
        SimpleAttributeSet attrs=new SimpleAttributeSet();
        StyleConstants.setAlignment(attrs,StyleConstants.ALIGN_CENTER);

        JButton clear_scores  = new JButton("Clear ratings");

        //Split for left and right parts
        JSplitPane rightPanel_split  = new JSplitPane(JSplitPane.VERTICAL_SPLIT,rightPanel,clear_scores);
        rightPanel_split.setResizeWeight(0.943);


        JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel_split);
        split.setResizeWeight(0.142);
        frame.add(split);

        //listener for enabling prediction calculation
        table.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                TableCellEditor tce = table.getCellEditor();
                if(tce != null){
                    int j = 0;
                    for (int i = 0; i < 30; i++) {
                        Movie scoredMovie = model.modelList.get(i);
                        if (scoredMovie.score>0){j+=1; }
                    }
                    if (j>9){
                        runPrediction.setEnabled(true);
                    }
                }
            }
        });

        //running prediction
        String finalSMatrixPath = sMatrixPath;
        runPrediction.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {

                output.setText("");
                output.setBackground(new ColorUIResource(36, 34, 34));

                //READING SCORES
                HashMap<String, Double> usersScores = new HashMap<>();

                for (int i = 0; i < 30; i++) {
                    Movie scoredMovie = model.modelList.get(i);
                    usersScores.put(scoredMovie.getId(), scoredMovie.getScore()/2);
                }

                Recommendation recommendation = new Recommendation(usersScores, finalSMatrixPath);
                try {

                    Map<String, Double> result = recommendation.calculateRecommendation(10);


                    int i = 1;
                    for (String bestId:result.keySet()) {
                        String title = allTitles.get(bestId);
                        Document ins = output.getDocument();
                        if (i<10) {
                            ins.insertString(ins.getLength(), String.valueOf(i) + ".   " + title + "\n", outputText);
                        }else {
                            ins.insertString(ins.getLength(), String.valueOf(i) + ". " + title, outputText);
                        }
                        i += 1;
                    }

                    result.clear();

                } catch (IOException | BadLocationException e1) {
                    e1.printStackTrace();
                }
            }
        });

        //clear button action
        clear_scores.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                for (int i = 0; i < 30; i++) {

                    model.setValueAt(0.0,i,2);

                }
                model.fireTableRowsUpdated(0,29);
                runPrediction.setEnabled(false);
            }});

        frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        frame.setVisible(true);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

}
