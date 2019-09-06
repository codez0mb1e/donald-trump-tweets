//*****************************************************************************************
//*                                                                                       *
//* This is an auto-generated file by Microsoft ML.NET CLI (Command-Line Interface) tool. *
//*                                                                                       *
//*****************************************************************************************

using Microsoft.ML.Data;

namespace DonaldTrumpTweets.ML.Model.DataModels
{
    public class ModelInput
{
    [ColumnName("created_date"), LoadColumn(0)]
    public string Created_date { get; set; }


    [ColumnName("n"), LoadColumn(1)]
    public float N { get; set; }


    [ColumnName("sentiment_score_min"), LoadColumn(2)]
    public float Sentiment_score_min { get; set; }


    [ColumnName("sentiment_score_mean"), LoadColumn(3)]
    public float Sentiment_score_mean { get; set; }


    [ColumnName("sentiment_score_max"), LoadColumn(4)]
    public float Sentiment_score_max { get; set; }


    [ColumnName("retweet_count_total"), LoadColumn(5)]
    public float Retweet_count_total { get; set; }


    [ColumnName("retweet_count_median"), LoadColumn(6)]
    public float Retweet_count_median { get; set; }


    [ColumnName("favorite_count_total"), LoadColumn(7)]
    public float Favorite_count_total { get; set; }


    [ColumnName("favorite_count_median"), LoadColumn(8)]
    public float Favorite_count_median { get; set; }


    [ColumnName("text_total"), LoadColumn(9)]
    public string Text_total { get; set; }


    [ColumnName("direction"), LoadColumn(10)]
    public bool Direction { get; set; }


}
}
