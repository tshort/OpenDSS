using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Reflection;
using System.Diagnostics;
using OpenDSSengine;
using Microsoft.Office.Interop.Excel;

namespace DemoOpenDSS
{
   public partial class DemoOpenDSSUI : Form
   {
      public DSS DSSobj;
      public Text DSSText;
      public Circuit DSSCircuit;
      public Solution DSSSolution;
      public CtrlQueue DSSControlQueue;

      public static _Application application = new Microsoft.Office.Interop.Excel.Application();
      public static string pathApp = Path.GetDirectoryName( Assembly.GetExecutingAssembly().Location );

      public static string filePath = pathApp + @"\results.xlsx";
      public _Workbook workbook = application.Workbooks.Open( filePath, Type.Missing, Type.Missing, Type.Missing,
                                             Type.Missing, Type.Missing, Type.Missing, Type.Missing,
                                             Type.Missing, Type.Missing, Type.Missing, Type.Missing,
                                             Type.Missing, Type.Missing, Type.Missing );
           
      
      public DemoOpenDSSUI()
      {
         InitializeComponent();
      }


      private void starDSS_Click( object sender, EventArgs e )
      {
         DSSobj = new DSS();
         if ( !( DSSobj.Start( 0 ) ) )
         {
            MessageBox.Show( "DSS failed to start" );
         }
         else
         {
            MessageBox.Show( "DSS started sucessfully" );
            DSSText = DSSobj.Text;
         }
      }


      private void loadCircuit_Click( object sender, EventArgs e )
      {
         string circuitName = textBox1.Text;
                  
         DSSText.Command = "clear";
         DSSText.Command = "compile " + "(" + circuitName +")";
         DSSCircuit = DSSobj.ActiveCircuit;
         DSSSolution = DSSCircuit.Solution;
         DSSControlQueue = DSSCircuit.CtrlQueue;

         MessageBox.Show( "Circuit Loaded" );

      }

      private void solveCircuit_Click( object sender, EventArgs e )
      {
         DSSSolution.Solve();
         if ( DSSSolution.Converged )
            MessageBox.Show( "Solution Converged" );
         else
            MessageBox.Show( "Solution did not converge" );
      }

      private void loadSeqVoltages_Click( object sender, EventArgs e )
      {
         Bus DSSBus;
         int iRow, iCol, i, j;
         dynamic V, cpxV;
       
         _Worksheet worksheet = (Worksheet) workbook.Worksheets["SeqVoltSheet"];

         iRow = 2;

         for ( i = 1 ; i <= DSSCircuit.NumBuses ; i++ )
         {
            DSSBus = DSSCircuit.Buses[i];
            worksheet.Cells[iRow, 1].Value = DSSCircuit.ActiveBus.Name;

            V = DSSBus.SeqVoltages;
            cpxV = DSSBus.CplxSeqVoltages;

            iCol = 2;
            for ( j = 0 ; j < V.Length ; j++ ) 
            {
               worksheet.Cells[iRow, iCol].Value = V[j];
               iCol = iCol + 1;
            }

            iCol = 6;
            for ( j = 0 ; j < cpxV.Length ; j++ )
            {
               worksheet.Cells[iRow, iCol].Value = cpxV[j];
               iCol = iCol + 1;
            }


            iRow = iRow + 1;
         
         }

         workbook.Save();
     }

      private void loadVoltages_Click( object sender, EventArgs e )
      {
         Bus DSSBus;
         int iRow, iCol, i, j;
         dynamic V;

         _Worksheet worksheet = ( Worksheet ) workbook.Worksheets["VoltSheet"];

         iRow = 2;

         for ( i = 1 ; i <= DSSCircuit.NumBuses ; i++ )
         {
            DSSBus = DSSCircuit.Buses[i];
            worksheet.Cells[iRow, 1].Value = DSSCircuit.ActiveBus.Name;

            V = DSSBus.puVoltages;

            iCol = 2;
            for ( j = 0 ; j < V.Length ; j++ )
            {
               worksheet.Cells[iRow, iCol].Value = V[j];
               iCol = iCol + 1;
            }

            iRow = iRow + 1;

         }

         workbook.Save();

      }

      private void loadMagAngle_Click( object sender, EventArgs e )
      {
         Bus DSSBus;
         int iRow, iCol, i, j;
         dynamic V;

         _Worksheet worksheet = ( Worksheet ) workbook.Worksheets["MagSheet"];

         iRow = 2;

         for ( i = 1 ; i <= DSSCircuit.NumBuses ; i++ )
         {
            DSSBus = DSSCircuit.Buses[i];
            worksheet.Cells[iRow, 1].Value = DSSCircuit.ActiveBus.Name;

            V = DSSBus.puVoltages;

            iCol = 2;
            for ( j = 0 ; j < V.Length ; j=j+2 )
            {
               worksheet.Cells[iRow, iCol].Value = Math.Sqrt(V[j]*V[j]+V[j+1]*V[j+1]);
               worksheet.Cells[iRow, iCol + 1].Value = angleC(V[j],V[j+1]);
               iCol = iCol + 2;
            }

            iRow = iRow + 1;

         }

         workbook.Save();

      }

      public static double angleC( double Re, double Im )
      {
         double angle;
         if ( Re != 0 )
         {
            angle = Math.Atan( Im / Re ) * 180 / Math.PI;
            if ( Re < 0 )
            {
               angle = angle + 180;
               return angle;
            }
            else 
               return angle;
         }
         else 
         {
            if ( Im > 0 )
               return 90;
            else if ( Im < 0 )
               return -90;
            else
               return 0;
            }      
      }

      private void loadSeqCurrents_Click( object sender, EventArgs e )
      {
         CktElement DSSElement;
         int iRow, iCol, i, j;
         dynamic SeqCurr, cpxSeqValues;

         _Worksheet worksheet = ( Worksheet ) workbook.Worksheets["SeqCurrSheet"];


         DSSElement = DSSCircuit.ActiveCktElement;

         iRow = 3;
         i = DSSCircuit.FirstPDElement();

         while (i>0)
         {
            if ( DSSElement.NumPhases == 3 )
            {
               worksheet.Cells[iRow, 1].Value = DSSElement.Name;

               SeqCurr = DSSElement.SeqCurrents;
               cpxSeqValues = DSSElement.CplxSeqCurrents;

               iCol = 2;
               for ( j = 0 ; j < SeqCurr.Length ; j++ )
               {
                  worksheet.Cells[iRow, iCol].Value = SeqCurr[j];
                  iCol = iCol + 1;
               }

               iCol = 9;
               for ( j = 0 ; j < cpxSeqValues.Length ; j++ )
               {
                  worksheet.Cells[iRow, iCol].Value = cpxSeqValues[j];
                  iCol = iCol + 1;
               }
               iRow = iRow + 1;
            }
            i = DSSCircuit.NextPDElement();
         }

         workbook.Save();

      }

      private void loadLoadPowers_Click( object sender, EventArgs e )
      {
         CktElement DSSElement;
         int iRow, iCol, i, j;
         dynamic PCPowers;
         double SumkW, Sumkvar;

         _Worksheet worksheet = ( Worksheet ) workbook.Worksheets["LoadPowers"];

         DSSElement = DSSCircuit.ActiveCktElement;

         iRow = 2;
         i = DSSCircuit.Loads.First;

         while ( i > 0 )
         {
               worksheet.Cells[iRow, 1].Value = DSSElement.Name;

               PCPowers = DSSElement.Powers;

               iCol = 2;
               SumkW = 0;
               Sumkvar = 0;

               for ( j = 0 ; j < PCPowers.Length ; j=j+2 )
               {
                  SumkW = SumkW + PCPowers[j];
                  Sumkvar = Sumkvar + PCPowers[j+1];
               }

               worksheet.Cells[iRow, iCol].Value = SumkW;
               worksheet.Cells[iRow, iCol+1].Value = Sumkvar;

               iRow = iRow + 1;
            i = DSSCircuit.Loads.Next;
         }

         workbook.Save();
      }

      private void DemoOpenDSSUI_FormClosed( object sender, FormClosedEventArgs e )
      {
         if ( workbook != null )
            workbook.Close( false, Type.Missing, Type.Missing );
         workbook = null;
         if ( application != null )
            application.Quit();
         application = null;
      }


   }
}
