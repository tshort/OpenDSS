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

namespace WindowsFormsApplication2
{
    public partial class Form1 : Form
    {
        public DSS DSSObj;
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            DSSObj = new DSS();
            if (DSSObj.Start(0))
            {
              MessageBox.Show("OpenDSS Started OK"); 
            };


        }
    }
}
