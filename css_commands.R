#css and js commands

css <- '
body {
  font-family: Roboto, sans-serif;
  text-align: justify;
}
p {
      font-family: Roboto, sans-serif;
}
a {
      font-family: Roboto, sans-serif;
    }
h1, h2, h3, h4, h5, h6 {
      font-family: Roboto, sans-serif;
    }
.content-wrapper {background-color: #F2F3F4;}
.tooltip {
pointer-events: none;
}
.tooltip > .tooltip-inner {
pointer-events: none;
background-color: #FBFCFC;
color: #17202A;
border: 1px solid black;
padding: 10px;
font-size: 12px;
font-style: italic;
text-align: left;
margin-left: 0;
width: 300px;
max-width: 1000px;
}
.tooltip > .arrow::before {
border-right-color: #73AD21;
}
.bolder-text {
        font-family: Roboto, sans-serif;
        font-weight: 1000; /* This makes the text bolder */
        -webkit-text-stroke: 0.3px black; /* Adjust the width and color for desired effect */
        color: black; /* Text color */
}
#clay_slider,#clay_slider2,#MR_slider,#HR_slider {
          margin-left: 5%;
}
#downloadData_FixedLU,#downloadData_LUchange,#downloadData_LM {
width: 100%; /* Set the width to 100% of the sidebar */
height: 60px; /* Set the desired height */
margin-top: 5px; /* Adjust top margin for spacing */
text-align: center; /* Center the button text */
background-color: #E5E4E2; /* Set the desired background color */
}
#downloadData_FixedLU:hover,#downloadData_LUchange:hover,#downloadData_LM:hover {
background-color: #D3D3D3; /* Set the background color on hover */
}
table {
        border-collapse: collapse;
        width: 100%;
}
th, td {
        border: 1px solid #dddddd;
        padding: 8px;
        text-align: center;
}
th {
        background-color: #f2f2f2;
}
'

js <- "
$(function () {
$('[data-toggle=tooltip]').tooltip()
})
"
