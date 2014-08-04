using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        
        private readonly Main.GameOfLife.Game game = new Main.GameOfLife.Game();

        private readonly IEnumerable<Tuple<Tuple<int, int>, Label>> cells =
            Enumerable.Range(0, 15)
                .Select(x => Enumerable.Range(0, 15)
                    .Select(y =>
                    {
                        var label = new Label();

                        label.Text = "X";

                        label.Left = x * 17;
                        label.Top = y * 17;
                        label.Width = 17;
                        label.Height = 17;

                        return Tuple.Create(Tuple.Create(x,y), label);
                    }))
                .SelectMany(x => x)
                .ToArray();

        private int _iterations = 0;
        private int Iterations
        {
            get
            {
                return _iterations;
            }
            set
            {
                if(value > 0 && value < int.MaxValue)
                {
                    _iterations = value;
                    
                    iterationLbl.Text = value.ToString();
                    
                    var played = game.Play(value);

                    foreach (var row in played)
                    {
                        foreach (var column in row)
                        {
                            cells.Single(x =>
                                x.Item1.Item1 == column.position.Item1 &&
                                x.Item1.Item2 == column.position.Item2).Item2.Text = column.state == Main.GameOfLife.State.Alive ? "O" : "X";
                        }
                    }
                }
            }
        }


        public Form1()
        {
            InitializeComponent();

            this.Controls.AddRange(cells.Select(x => x.Item2).Cast<Control>().ToArray());
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Iterations--;
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Iterations++;
        }
    }
}
