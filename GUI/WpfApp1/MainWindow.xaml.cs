using Interpreter;
using Microsoft.FSharp.Collections;
using OxyPlot;
using OxyPlot.Axes;
using OxyPlot.Series;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace WpfApp1
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    /// 
    public partial class MainWindow : Window
    {

        private Microsoft.FSharp.Collections.FSharpMap<string, Interpreter.Interpreter.assignmentTypes> symbolState;

        public PlotModel MyModel { get; private set; }
        private string currentPlotExpression;
        private double lastDataXMin = 0;
        private double lastDataXMax = 0;

        private bool isPlotting = false;

        public MainWindow()
        {
            InitializeComponent();

            BnfTextBox.Text = "Grammar in BNF: \r\n<Statement> ::= <Id> \"(\" <Id> \")\" \"=\" <E> \";\" | <Id> \"=\" <E> \";\" | \"for\" <Id> \"=\" <E> \"to\" <E> \";\" | <E> \";\" | <E>\r\n<E> ::= <T> <Eopt>\r\n<Eopt> ::= \"+\" <T> <Eopt> | \"-\" <T> <Eopt> | <empty>\r\n<T> ::= <P> <Topt>\r\n<Topt> ::= \"*\" <P> <Topt> | \"/\" <P> <Topt> | \"%\" <P> <Topt> | <empty>\r\n<P> ::= <NR> <Popt>\r\n<Popt> ::= \"^\" <P> | <empty>\r\n<NR> ::= \"Num\" <value> |  <Id> \"(\" <E> \")\" | <Id> | \"-\" <NR> | \"(\" <E> \")\" | \"cos\" \"(\" <E> \")\" | \"sin\" \"(\" <E> \")\" | \"tan\" \"(\" <E> \")\" | \"exp\" \"(\" <E> \")\" | \"log\" \"(\" <E> \")\"";

            symbolState = Interpreter.Interpreter.InitialSymTab;

            // Initialize OxyPlot
            this.MyModel = new PlotModel { Title = "Function Plot" };
            MyModel.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Bottom,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot
            });

            MyModel.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Left,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot
            });

            this.DataContext = this;

            OxyPlotView.PreviewMouseUp += OxyPlotView_PreviewMouseUp;
            OxyPlotView.PreviewMouseWheel += OxyPlotView_PreviewMouseWheel;
        }

        class Global
        {
            public static int attempts = 0;
            public static bool darkMode = false;

            public static string lastInput = "";

            public static Stack<string> undoHistory = new Stack<string>();
            public static Stack<string> redoHistory = new Stack<string>();
        }
        private void Interpret(object sender, RoutedEventArgs e)
        {
            Global.attempts++;

            Tab.SelectedIndex = 0;

            string input = MyTextBox.Text;

            if (Global.lastInput != input)
            {
                if (Global.lastInput != null)
                {
                    Global.undoHistory.Push(Global.lastInput);
                }
                Global.redoHistory.Clear();
            }

            var resultTuple = Interpreter.Interpreter.CSharpInput(symbolState, input);

            symbolState = resultTuple.Item1;

            string result = resultTuple.Item2;

            Brush colour;
            if (result.ToLower().Contains("error"))
            {
                colour = Brushes.Red;
            }
            else if (Global.darkMode == true)
            {
                colour = Brushes.White;
            } else {
                colour = Brushes.Black;
            }

            if (MyTextBlock.Text != "")
            {
                MyTextBlock.Inlines.Add(new LineBreak());
            }

            Run resultRun;

            if (result == "")
            {
                resultRun = new Run("[" + Global.attempts + "]: " + input);
            } else
            {
                resultRun = new Run("[" + Global.attempts + "]: " + input + System.Environment.NewLine + result);
            }

            System.Console.WriteLine(symbolState);

            resultRun.Foreground = colour;

            MyTextBlock.Inlines.Add(resultRun);

            MyScrollViewer.ScrollToEnd();

            Global.lastInput = input;
        }

        private void DarkMode(object sender, RoutedEventArgs e)
        {
            Brush newForeground;
            Brush newBackground;
            OxyColor oxyForeground;
            OxyColor oxyBackground;

            if (Global.darkMode == true)
            {
                Global.darkMode = false;
                newBackground = Brushes.White;
                newForeground = Brushes.Black;
                oxyForeground = OxyColors.Black;
                oxyBackground = OxyColors.White;
            }
            else
            {
                Global.darkMode = true;
                newBackground = Brushes.Black;
                newForeground = Brushes.White;
                oxyForeground = OxyColors.White;
                oxyBackground = OxyColors.Black;
            }
 
            this.Background = newBackground;
            MyTextBox.Background = newBackground;
            MyTextBox.Foreground = newForeground;

            BnfTextBox.Background = newBackground;
            BnfTextBox.Foreground = newForeground;

            HelpTextBox.Background = newBackground;
            HelpTextBox.Foreground = newForeground;

            Tab.Background = newBackground;
            MyTextBlock.Background = newBackground;

            if (this.MyModel != null)
            {
                this.MyModel.Background = oxyBackground; 
                this.MyModel.TextColor = oxyForeground;
                this.MyModel.PlotAreaBorderColor = oxyForeground;

                foreach (var axis in this.MyModel.Axes)
                {
                    axis.AxislineColor = oxyForeground;
                    axis.TextColor = oxyForeground;
                    axis.TicklineColor = oxyForeground;
                    axis.TitleColor = oxyForeground;

                    axis.MajorGridlineColor = Global.darkMode ? OxyColor.FromAColor(40, OxyColors.White) : OxyColor.FromAColor(40, OxyColors.Black);
                    axis.MinorGridlineColor = Global.darkMode ? OxyColor.FromAColor(20, OxyColors.White) : OxyColor.FromAColor(20, OxyColors.Black);
                }

                this.MyModel.InvalidatePlot(false);
            }

            foreach (Inline inline in MyTextBlock.Inlines)
            {
                if (inline is Run run)
                {
                    SolidColorBrush currentBrush = run.Foreground as SolidColorBrush;
                    if (currentBrush == null || currentBrush.Color != Colors.Red)
                    {
                        run.Foreground = newForeground;
                    }
                }
            }
        }

        private void Undo(object sender, RoutedEventArgs e)
        {
            if (Global.undoHistory.Count > 0)
            {
                Global.redoHistory.Push(MyTextBox.Text);

                MyTextBox.Text = Global.undoHistory.Pop();

                Global.lastInput = MyTextBox.Text;

            }
        }

        private void Redo(object sender, RoutedEventArgs e)
        {
            if (Global.redoHistory.Count > 0)
            {
                Global.undoHistory.Push(MyTextBox.Text);

                MyTextBox.Text = Global.redoHistory.Pop();

                Global.lastInput = MyTextBox.Text;
            }
        }
        private void Plot(object sender, RoutedEventArgs e)
        {
            string input = MyTextBox.Text;

            var checkResult = Interpreter.Interpreter.CSharpInput(symbolState, input);
            string resultText = checkResult.Item2;

            if (resultText.ToLower().Contains("error"))
            {
                var constantCheck = Interpreter.Interpreter.CSharpPlotToNullable(symbolState, input, "x", 0);

                if (!constantCheck.HasValue)
                {
                    Tab.SelectedIndex = 0;
                    MessageBox.Show($"Cannot plot: {resultText}", "Validation Error", MessageBoxButton.OK, MessageBoxImage.Error);
                    return;
                }
            }

            if (MyModel.Axes.Count > 0)
            {
                foreach (var axis in MyModel.Axes) axis.Reset();
            }
            Tab.SelectedIndex = 1;

            this.currentPlotExpression = input;
            this.MyModel.Title = $"Plot of: {input}";

            PlotFunction(-10, 10);

            if (Global.lastInput != input)
            {
                if (Global.lastInput != null) Global.undoHistory.Push(Global.lastInput);
                Global.redoHistory.Clear();
            }
            Global.lastInput = input;
        }

        private async void PlotFunction(double xMin, double xMax)
        {
            lastDataXMin = xMin;
            lastDataXMax = xMax;

            string expression = currentPlotExpression;
            var currentSymbolState = symbolState;

            var points = await Task.Run(() =>
            {
                var resultPoints = new List<DataPoint>();
                const int numPoints = 1000; 
                double step = (xMax - xMin) / numPoints;

                for (int i = 0; i <= numPoints; i++)
                {
                    double x = xMin + i * step;
                    try
                    {
                        var yValue = Interpreter.Interpreter.CSharpPlotToNullable(currentSymbolState, expression, "x", x);

                        if (yValue.HasValue && !double.IsNaN(yValue.Value) && !double.IsInfinity(yValue.Value))
                        {
                            double y = yValue.Value;
                            if (y >= -1e6 && y <= 1e6)
                            {
                                resultPoints.Add(new DataPoint(x, y));
                            }
                        }
                    }
                    catch
                    {
                        continue;
                    }
                }
                return resultPoints;
            });

            var series = new LineSeries { Title = "f(x)", Color = OxyColors.Blue, StrokeThickness = 2 };
                foreach (var pt in points) series.Points.Add(pt);

                MyModel.Series.Clear();
                MyModel.Series.Add(series);

            // Auto-scale Y
            var yAxis = MyModel.Axes.OfType<LinearAxis>().FirstOrDefault(a => a.Position == AxisPosition.Left);
            if (yAxis != null && points.Count > 0)
            {
                double yMin = points.Min(p => p.Y);
                double yMax = points.Max(p => p.Y);

                if (Math.Abs(yMax - yMin) < 1e-6) 
                {
                    yAxis.Minimum = yMin - 5; 
                    yAxis.Maximum = yMax + 5;
                }
                else
                {
                    double padding = (yMax - yMin) * 0.1;
                    yAxis.Minimum = yMin - padding;
                    yAxis.Maximum = yMax + padding;
                }
            }

            MyModel.InvalidatePlot(true);
            }


        private void OxyPlotView_PreviewMouseUp(object sender, System.Windows.Input.MouseButtonEventArgs e)
        {
            if (string.IsNullOrEmpty(currentPlotExpression)) return;

            if (e.ChangedButton == System.Windows.Input.MouseButton.Right)
            {
                var xAxis = MyModel.Axes.OfType<LinearAxis>().FirstOrDefault(a => a.Position == AxisPosition.Bottom);

                if (xAxis != null && !double.IsNaN(xAxis.ActualMinimum) && !double.IsNaN(xAxis.ActualMaximum))
                {
                    double visibleXMin = xAxis.ActualMinimum;
                    double visibleXMax = xAxis.ActualMaximum;

                    if (visibleXMin < lastDataXMin || visibleXMax > lastDataXMax)
                    {
                        double visibleRange = visibleXMax - visibleXMin;
                        double newXMin = visibleXMin - visibleRange * 0.1;
                        double newXMax = visibleXMax + visibleRange * 0.1;

                        PlotFunction(newXMin, newXMax);
                    }
                }
            }
        }

        private void OxyPlotView_PreviewMouseWheel(object sender, System.Windows.Input.MouseWheelEventArgs e)
        {
            if (isPlotting || string.IsNullOrEmpty(currentPlotExpression)) return;

            var xAxis = MyModel.Axes.OfType<LinearAxis>().FirstOrDefault(a => a.Position == AxisPosition.Bottom);

            if (xAxis != null && !double.IsNaN(xAxis.ActualMinimum) && !double.IsNaN(xAxis.ActualMaximum))
            {
                double visibleXMin = xAxis.ActualMinimum;
                double visibleXMax = xAxis.ActualMaximum;
                double visibleRange = visibleXMax - visibleXMin;

                if (visibleRange <= 0) return;

                double dataRange = lastDataXMax - lastDataXMin;
                if (dataRange <= 0) return; 

                double margin = dataRange * 0.2;
                bool isOutsideBounds = visibleXMin < lastDataXMin + margin || visibleXMax > lastDataXMax - margin;
                bool isZoomedInDeeply = visibleRange < dataRange * 0.5;

                if (isOutsideBounds || isZoomedInDeeply)
                {
                    isPlotting = true;
                    double newXMin = visibleXMin - visibleRange * 0.5;
                    double newXMax = visibleXMax + visibleRange * 0.5;

                    Dispatcher.BeginInvoke(new Action(() => {
                        try { PlotFunction(newXMin, newXMax); }
                        finally { isPlotting = false; }
                    }));
                }
            }
        }
    }
}
