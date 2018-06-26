
function updateFeild(){
  var e = document.getElementById('drop');
  var strUser = e.options[e.selectedIndex].text;
  var elements = $('div.container').children().hide();
  var value = $(this).val();
  elements.filter('.' + value).show(); 
}

function updateOtherFeild(){
   var value = $(this).val();
   document.getElementById('abc').value = value;
}

function onLoad()
{
  document.getElementById('abc').value = 'WORK';
}

$(document).ready(onLoad);
