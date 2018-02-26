public class GeniusPlayer extends PcPlayer {
	Deck deck;
	GeniusPlayer() {
		//;
	}

	@Override
	public void doTurn() {
		if(r.nextFloat() <= 0.5)
			stopPlaying();
		// тут мы будет новая модель
	}
}
