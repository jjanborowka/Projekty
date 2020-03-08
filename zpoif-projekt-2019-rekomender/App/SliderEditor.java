import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ColorUIResource;
import javax.swing.table.TableCellEditor;
import java.awt.*;

public class SliderEditor extends AbstractCellEditor implements TableCellEditor {
    private JPanel panel;
    private JSlider slider;
    private JTextArea txt;

    SliderEditor() {
        panel = new JPanel();
        panel.setLayout(new GridBagLayout());
        slider = new JSlider(SwingConstants.HORIZONTAL,0,10, 0);
        slider.setSize(600, 50);
        txt  = new JTextArea("");

        txt.setBackground(new ColorUIResource(238,238,238));
        panel.add(slider);
        panel.add(txt);
        panel.setVisible(true);
    }

    public Component getTableCellEditorComponent(JTable table, Object value,
                                                 boolean isSelected, int row, int column) {
        slider.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                JSlider source = (JSlider) e.getSource();
                txt.setText("" + source.getValue());
            }
        });
        slider.setValue(((Double) value).intValue());


        return panel;
    }

    public Object getCellEditorValue() {
        return (double) slider.getValue();
    }

}
