$(document).ready(function(){
  var i3 = $('#i3');
  var date = $('#date');
  var update = function(){
    $.get('/i3status',function(html){
      i3.html(html);
    });
    date.text(new Date());
  };
  update();
  setInterval(update,1000);
});
