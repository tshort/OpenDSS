namespace DemoOpenDSS
{
   partial class DemoOpenDSSUI
   {
      /// <summary>
      /// Required designer variable.
      /// </summary>
      private System.ComponentModel.IContainer components = null;

      /// <summary>
      /// Clean up any resources being used.
      /// </summary>
      /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
      protected override void Dispose( bool disposing )
      {
         if ( disposing && ( components != null ) )
         {
            components.Dispose();
         }
         base.Dispose( disposing );
      }

      #region Windows Form Designer generated code

      /// <summary>
      /// Required method for Designer support - do not modify
      /// the contents of this method with the code editor.
      /// </summary>
      private void InitializeComponent()
      {
         this.startDSS = new System.Windows.Forms.Button();
         this.textBox1 = new System.Windows.Forms.TextBox();
         this.label1 = new System.Windows.Forms.Label();
         this.loadCircuit = new System.Windows.Forms.Button();
         this.solveCircuit = new System.Windows.Forms.Button();
         this.loadSeqVoltages = new System.Windows.Forms.Button();
         this.loadMagAngle = new System.Windows.Forms.Button();
         this.loadSeqCurrents = new System.Windows.Forms.Button();
         this.loadLoadPowers = new System.Windows.Forms.Button();
         this.loadVoltages = new System.Windows.Forms.Button();
         this.SuspendLayout();
         // 
         // startDSS
         // 
         this.startDSS.Font = new System.Drawing.Font( "Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ( ( byte ) ( 0 ) ) );
         this.startDSS.Location = new System.Drawing.Point( 100, 22 );
         this.startDSS.Name = "startDSS";
         this.startDSS.Size = new System.Drawing.Size( 112, 38 );
         this.startDSS.TabIndex = 0;
         this.startDSS.Text = "Start DSS";
         this.startDSS.UseVisualStyleBackColor = true;
         this.startDSS.Click += new System.EventHandler( this.starDSS_Click );
         // 
         // textBox1
         // 
         this.textBox1.Location = new System.Drawing.Point( 120, 109 );
         this.textBox1.Name = "textBox1";
         this.textBox1.Size = new System.Drawing.Size( 410, 20 );
         this.textBox1.TabIndex = 1;
         this.textBox1.Text = "C:\\Program Files\\OpenDSS\\IEEETestCases\\123Bus\\IEEE123Master.DSS";
         // 
         // label1
         // 
         this.label1.AutoSize = true;
         this.label1.Font = new System.Drawing.Font( "Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ( ( byte ) ( 0 ) ) );
         this.label1.Location = new System.Drawing.Point( 13, 112 );
         this.label1.Name = "label1";
         this.label1.Size = new System.Drawing.Size( 101, 13 );
         this.label1.TabIndex = 2;
         this.label1.Text = "File Path Name :";
         // 
         // loadCircuit
         // 
         this.loadCircuit.Font = new System.Drawing.Font( "Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ( ( byte ) ( 0 ) ) );
         this.loadCircuit.Location = new System.Drawing.Point( 100, 179 );
         this.loadCircuit.Name = "loadCircuit";
         this.loadCircuit.Size = new System.Drawing.Size( 225, 38 );
         this.loadCircuit.TabIndex = 3;
         this.loadCircuit.Text = "Compile the Circuit File";
         this.loadCircuit.UseVisualStyleBackColor = true;
         this.loadCircuit.Click += new System.EventHandler( this.loadCircuit_Click );
         // 
         // solveCircuit
         // 
         this.solveCircuit.Font = new System.Drawing.Font( "Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ( ( byte ) ( 0 ) ) );
         this.solveCircuit.Location = new System.Drawing.Point( 100, 247 );
         this.solveCircuit.Name = "solveCircuit";
         this.solveCircuit.Size = new System.Drawing.Size( 225, 38 );
         this.solveCircuit.TabIndex = 4;
         this.solveCircuit.Text = "Solve the Circuit";
         this.solveCircuit.UseVisualStyleBackColor = true;
         this.solveCircuit.Click += new System.EventHandler( this.solveCircuit_Click );
         // 
         // loadSeqVoltages
         // 
         this.loadSeqVoltages.Font = new System.Drawing.Font( "Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ( ( byte ) ( 0 ) ) );
         this.loadSeqVoltages.Location = new System.Drawing.Point( 100, 383 );
         this.loadSeqVoltages.Name = "loadSeqVoltages";
         this.loadSeqVoltages.Size = new System.Drawing.Size( 225, 38 );
         this.loadSeqVoltages.TabIndex = 5;
         this.loadSeqVoltages.Text = "Load the Seq Voltages";
         this.loadSeqVoltages.UseVisualStyleBackColor = true;
         this.loadSeqVoltages.Click += new System.EventHandler( this.loadSeqVoltages_Click );
         // 
         // loadMagAngle
         // 
         this.loadMagAngle.Font = new System.Drawing.Font( "Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ( ( byte ) ( 0 ) ) );
         this.loadMagAngle.Location = new System.Drawing.Point( 100, 451 );
         this.loadMagAngle.Name = "loadMagAngle";
         this.loadMagAngle.Size = new System.Drawing.Size( 225, 38 );
         this.loadMagAngle.TabIndex = 6;
         this.loadMagAngle.Text = "Load the Mag/Angle Sheet";
         this.loadMagAngle.UseVisualStyleBackColor = true;
         this.loadMagAngle.Click += new System.EventHandler( this.loadMagAngle_Click );
         // 
         // loadSeqCurrents
         // 
         this.loadSeqCurrents.Font = new System.Drawing.Font( "Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ( ( byte ) ( 0 ) ) );
         this.loadSeqCurrents.Location = new System.Drawing.Point( 100, 519 );
         this.loadSeqCurrents.Name = "loadSeqCurrents";
         this.loadSeqCurrents.Size = new System.Drawing.Size( 225, 38 );
         this.loadSeqCurrents.TabIndex = 7;
         this.loadSeqCurrents.Text = "Load the Seq Currents";
         this.loadSeqCurrents.UseVisualStyleBackColor = true;
         this.loadSeqCurrents.Click += new System.EventHandler( this.loadSeqCurrents_Click );
         // 
         // loadLoadPowers
         // 
         this.loadLoadPowers.Font = new System.Drawing.Font( "Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ( ( byte ) ( 0 ) ) );
         this.loadLoadPowers.Location = new System.Drawing.Point( 100, 587 );
         this.loadLoadPowers.Name = "loadLoadPowers";
         this.loadLoadPowers.Size = new System.Drawing.Size( 225, 38 );
         this.loadLoadPowers.TabIndex = 8;
         this.loadLoadPowers.Text = "Load the Load Powers";
         this.loadLoadPowers.UseVisualStyleBackColor = true;
         this.loadLoadPowers.Click += new System.EventHandler( this.loadLoadPowers_Click );
         // 
         // loadVoltages
         // 
         this.loadVoltages.Font = new System.Drawing.Font( "Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ( ( byte ) ( 0 ) ) );
         this.loadVoltages.Location = new System.Drawing.Point( 100, 315 );
         this.loadVoltages.Name = "loadVoltages";
         this.loadVoltages.Size = new System.Drawing.Size( 225, 38 );
         this.loadVoltages.TabIndex = 9;
         this.loadVoltages.Text = "Load the Voltages";
         this.loadVoltages.UseVisualStyleBackColor = true;
         this.loadVoltages.Click += new System.EventHandler( this.loadVoltages_Click );
         // 
         // DemoOpenDSSUI
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF( 6F, 13F );
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size( 600, 732 );
         this.Controls.Add( this.loadVoltages );
         this.Controls.Add( this.loadLoadPowers );
         this.Controls.Add( this.loadSeqCurrents );
         this.Controls.Add( this.loadMagAngle );
         this.Controls.Add( this.loadSeqVoltages );
         this.Controls.Add( this.solveCircuit );
         this.Controls.Add( this.loadCircuit );
         this.Controls.Add( this.label1 );
         this.Controls.Add( this.textBox1 );
         this.Controls.Add( this.startDSS );
         this.Name = "DemoOpenDSSUI";
         this.Text = "OpenDSS Demo";
         this.FormClosed += new System.Windows.Forms.FormClosedEventHandler( this.DemoOpenDSSUI_FormClosed );
         this.ResumeLayout( false );
         this.PerformLayout();

      }

      #endregion

      private System.Windows.Forms.Button startDSS;
      private System.Windows.Forms.TextBox textBox1;
      private System.Windows.Forms.Label label1;
      private System.Windows.Forms.Button loadCircuit;
      private System.Windows.Forms.Button solveCircuit;
      private System.Windows.Forms.Button loadSeqVoltages;
      private System.Windows.Forms.Button loadMagAngle;
      private System.Windows.Forms.Button loadSeqCurrents;
      private System.Windows.Forms.Button loadLoadPowers;
      private System.Windows.Forms.Button loadVoltages;
   }
}

