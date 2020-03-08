import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import javax.swing.table.TableCellRenderer;
import java.awt.*;

public class CellRenderer implements TableCellRenderer {
    JPanel panel;
    JSlider slider;
    JTextArea txt;

    public CellRenderer(){
        panel = new JPanel();
        panel.setLayout(new GridBagLayout());
        slider = new JSlider(SwingConstants.HORIZONTAL,0,10, 0);
        slider.setSize(600, 50);
        txt  = new JTextArea("0");

        slider.addChangeListener(e -> {
            JSlider source = (JSlider) e.getSource();
            txt.setText("" + source.getValue());
        });
        txt.setBackground(new ColorUIResource(238,238,238));
        panel.add(slider);
        panel.add(txt);
        panel.setVisible(true);

    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        Double val = (Double) value;
        slider.setValue(val.intValue());

        return panel;
    }
}
