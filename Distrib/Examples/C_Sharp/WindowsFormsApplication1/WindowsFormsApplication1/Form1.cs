using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using OpenDSSengine;


namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        public DSS DSSObj;
        public Text DSSText;
        public Circuit DSSCircuit;
        public Solution DSSSolution;

        public Form1()
        {
            InitializeComponent();

        }

        private void StartButton_Click(object sender, EventArgs e)
        {

            DSSObj = new DSS();
            if (!(DSSObj.Start(0)))
            {
                MessageBox.Show("DSS failed to start");
            }
            else
            {    //Successful start!
                 //  MessageBox.Show("DSS started sucessfully");

                // set some variables to interfaces to make coding easier
                DSSText = DSSObj.Text; // Set Variable for accessing the Text interface
                DSSCircuit = DSSObj.ActiveCircuit;
                DSSSolution = DSSCircuit.Solution;
                Form1.ActiveForm.Text = DSSObj.Version;
            }
            
        }

        private void solvebtn_Click(object sender, EventArgs e)
        {
            DSSSolution.Solve();
            if (!(DSSSolution.Converged)) 
            {
                MessageBox.Show("Solution failed.");
            }

        }

        private void ShowBtn_Click(object sender, EventArgs e)
        {
            DSSText.Command = "Show Voltages LN Nodes";
            DSSText.Command = "Show Currents Elem";
            DSSText.Command = "Show Power kVA elem";

           
        }

        private void compilebtn_Click(object sender, EventArgs e)
        {
            DSSText.Command = "Compile " + FileNameBox.Text;
            if (DSSObj.Error.Number > 0)
            {
                MessageBox.Show(DSSText.Result);
            }
            else
            {
                DSSText.Command = "New Energymeter.m1  Line.L115 1 ";
                MessageBox.Show("No Errors.");
                DSSText.Command = "? RegControl.creg4a.vreg";
                VregText.Text = DSSText.Result;
            }
        }

        private void ProfilePLot_Click(object sender, EventArgs e)
        {
            DSSText.Command = "Plot Profile";
        }

        private static double cabs(double R,double im)
        {
            return (Math.Sqrt(R * R + im * im));
        }

        private void Reg4Adjust_Click(object sender, EventArgs e)
        {
            Bus DSSBus;
            dynamic V;   // this is a variant
            int Regtap;

            DSSBus = DSSCircuit.ActiveBus;
            DSSCircuit.SetActiveBus("83");

            V = DSSBus.puVoltages;   // get the voltages at the active bus

            if (cabs((double)V[0], (double)V[1]) > 0.98)  // Phase a
            {
                DSSCircuit.RegControls.Name = "creg4a";
                Regtap = DSSCircuit.RegControls.TapNumber;
                if (Regtap > -16) 
                {
                    DSSCircuit.RegControls.TapNumber = Regtap - 1;  // drop the tap
                }
            };

            if (cabs((double)V[2], (double)V[3]) > 0.98)  // phase b
            {
                DSSCircuit.RegControls.Name = "creg4b";
                Regtap = DSSCircuit.RegControls.TapNumber;
                if (Regtap > -16)
                {
                    DSSCircuit.RegControls.TapNumber = Regtap - 1;
                }
            };

            if (cabs((double)V[4], (double)V[5]) > 0.98)  // phase c
            {
                DSSCircuit.RegControls.Name = "creg4c";
                Regtap = DSSCircuit.RegControls.TapNumber;
                if (Regtap > -16)
                {
                    DSSCircuit.RegControls.TapNumber = Regtap - 1;
                }

            };

            DSSCircuit.Solution.Solve();


        }

        private void button1_Click(object sender, EventArgs e)
        {
            DSSText.Command = richTextBox1.SelectedText;
            ResultBox.Text = DSSText.Result;
        }

        private void button2_Click(object sender, EventArgs e)
        {
            DSSObj.ShowPanel();
        }

        private void SetVregBtn_Click(object sender, EventArgs e)
        {

            Bus DSSBus;
            dynamic V;   // this is a variant
            
            DSSBus = DSSCircuit.ActiveBus;
            DSSCircuit.SetActiveBus("83");

            V = DSSBus.puVoltages;   // get the voltages at the active bus

            if (cabs((double)V[0], (double)V[1]) > 0.98)  // Phase a
            {
                DSSCircuit.RegControls.Name = "creg4a";

                DSSText.Command = "RegControl.Creg4a.vreg=" + VregText.Text;
                DSSText.Command = "RegControl.Creg4b.vreg=" + VregText.Text;
                DSSText.Command = "RegControl.Creg4c.vreg=" + VregText.Text;

                DSSCircuit.Solution.Solve();    

            };

            DSSText.Command = "? RegControl.creg4a.vreg";
            VregText.Text = DSSText.Result;

        }

        private void button3_Click(object sender, EventArgs e)
        {
            Loads DSSLoads;
            int pLoad;

            DSSLoads = DSSCircuit.Loads;


            // Add an EV to every other 1-phase load

            pLoad = DSSLoads.First;

            while (pLoad > 0 )
            {

                if (DSSCircuit.ActiveCktElement.NumPhases == 1)
                {
                    DSSLoads.kW = DSSLoads.kW + 10;
                }


                pLoad = DSSLoads.Next;
                pLoad = DSSLoads.Next;

            }


        }




    }
}
